package konstructs

import java.util.UUID
import java.io.{FileReader, FileNotFoundException, File}
import org.apache.commons.io.FileUtils
import scala.collection.JavaConverters._
import akka.actor.ActorSystem
import com.typesafe.config.ConfigFactory
import com.google.gson.reflect.TypeToken
import konstructs.api.{GsonDefault, Position}
import konstructs.plugin.PluginLoaderActor
import konstructs.shard.{ShardPosition, ShardActor}

object Main extends App {
  val conf =
    ConfigFactory.parseFile(new java.io.File("konstructs.conf")).withFallback(ConfigFactory.load())
  implicit val system = ActorSystem("main", conf)
  // Run any global backwards compatibility scripts
  // Plugins should handle their own backwards compatibility
  Compatibility.process()
  val loader = system.actorOf(PluginLoaderActor.props(conf), "plugin-loader")
  loader ! PluginLoaderActor.Start
}

object Compatibility {
  import ShardActor.{shardId, positionMappingFile}
  val TypeOfPositionMapping = new TypeToken[java.util.Map[String, UUID]]() {}.getType

  def process() {
    println("STARTING MIGRATION PROCESS, DON'T INTERRUPT!")
    handleOldUuidDB()
    println("MIGRATION PROCESS FINISHED")
  }

  val PositionRegex = """(-?\d*)-(-?\d*)-(-?\d*)""".r

  def parsePositionStr(posStr: String): Position = posStr match {
    case PositionRegex(x, y, z) => new Position(x.toInt, y.toInt, z.toInt)
  }

  /*
   * This "migration" only works if disk storage on default location was used
   */
  def handleOldUuidDB() {
    val gson = GsonDefault.getDefaultGson
    try {
      val fileName = s"meta/org/konstructs/block-manager/position-mapping.json"
      val reader = new FileReader(fileName)
      val mapping: java.util.Map[String, UUID] = gson.fromJson(reader, TypeOfPositionMapping)
      println("Read old position mappings file, will migrate")
      val grouped = mapping.asScala.groupBy {
        case (positionStr, uuid) =>
          val position = parsePositionStr(positionStr)
          ShardPosition(position)
      }
      val meta = new File("meta")
      for ((shard, positions) <- grouped) {
        val file = Storage.file(meta, positionMappingFile(shardId(shard)), "chunks", "json")
        if (file.exists())
          throw new IllegalStateException("Shard position mapping already exists!")
        Storage.write(meta,
                      positionMappingFile(shardId(shard)),
                      "chunks",
                      "json",
                      gson.toJson(positions.asJava, TypeOfPositionMapping).getBytes())
        println(s"Migrated mapping for $shard to $file")
      }
      println("Position mapping migration finished, will delete original mappings file")
      FileUtils.forceDelete(new File(fileName))
      println("Successfully deleted original mappings file")
    } catch {
      case e: FileNotFoundException =>
        println("No old position mapping file found, skipping position mapping migration.")
    }
  }
}
