package konstructs

import java.util.UUID
import java.awt.image.BufferedImage
import java.awt.Graphics
import java.io.ByteArrayOutputStream
import javax.imageio.ImageIO

import scala.collection.JavaConverters._
import scala.util.Try

import com.typesafe.config.{ Config => TypesafeConfig, ConfigValueType, ConfigObject }
import akka.actor.{ Actor, Props, ActorRef, Stash }

import com.google.gson.reflect.TypeToken

import konstructs.plugin.Plugin._
import konstructs.api._
import konstructs.api.messages._
import konstructs.plugin.{ListConfig, PluginConstructor, Config}

case class BlockFactoryImpl(blockTypeIdMapping: java.util.Map[BlockTypeId, Integer],
  wMapping: java.util.Map[Integer, BlockTypeId], blockTypes: java.util.Map[BlockTypeId, BlockType])
    extends BlockFactory {

  override def createBlock(uuid: UUID, w: Int, health: Int): Block = {
    val t = wMapping.get(w)
    new Block(uuid, t, Health.get(health))
  }

  override def createBlock(uuid: UUID, w: Int): Block = {
    val t = wMapping.get(w)
    new Block(uuid, t)
  }

  override def createBlock(w: Int): Block = {
    val t = wMapping.get(w)
    new Block(null, t)
  }

  private def validate[T](w: Integer, t: BlockTypeId): Integer =
    if(w != null) w else throw new IndexOutOfBoundsException(s"Block type $t is not registered")

  override def getW(block: Block) =
   validate(blockTypeIdMapping.get(block.getType), block.getType)

  override def getW(stack: Stack) =
    validate(blockTypeIdMapping.get(stack.getTypeId), stack.getTypeId)

  override def getW(typeId: BlockTypeId) =
    validate(blockTypeIdMapping.get(typeId), typeId)

  override def getBlockType(id: BlockTypeId): BlockType = {
    val t = blockTypes.get(id)
    if(t != null) t else throw new IndexOutOfBoundsException(s"Block type $id is not registered")
  }

  override def getBlockTypeId(w: Int) =
    wMapping.get(w)

  override def getBlockTypes() =
    blockTypes

  override def getWMapping() =
    wMapping

}

object BlockFactoryImpl {

    private def findFreeW(wMapping: java.util.Map[Integer, BlockTypeId]): Integer = {
    for(w <- 0 until 256) {
      if(!wMapping.containsKey(w)) {
        return w
      }
    }
    throw new IllegalStateException("No free w to allocate for new block type")
  }

  private def addBlockType(blockTypeIdMapping: java.util.Map[BlockTypeId, Integer],
    wMapping: java.util.Map[Integer, BlockTypeId],
    blockTypeMapping: java.util.Map[BlockTypeId, BlockType]): PartialFunction[(BlockTypeId, BlockType), Unit] = {
    case (t, bt) =>
      val foundW = blockTypeIdMapping.get(t)
      val w = if(foundW != null) {
        foundW
      } else {
        findFreeW(wMapping)
      }
      blockTypeIdMapping.put(t, w)
      wMapping.put(w, t)
      blockTypeMapping.put(t, bt)
  }

  def apply(defined: java.util.Map[Integer, BlockTypeId], configured: Seq[(BlockTypeId, BlockType)]): BlockFactoryImpl = {
    val wMapping = new java.util.HashMap[Integer, BlockTypeId]()
    val reverse = new java.util.HashMap[BlockTypeId, Integer]()
    val tMapping = new java.util.HashMap[BlockTypeId, BlockType]()

    for((w, tId) <- defined.asScala) {
      wMapping.put(w, tId)
      reverse.put(tId, w)
    }
    configured.map(addBlockType(reverse, wMapping, tMapping))
    apply(reverse, wMapping, tMapping)
  }
}

class BlockMetaActor( val ns: String,
                      val jsonStorage: ActorRef,
                      configuredBlocks: Seq[(BlockTypeId, BlockType)],
                      textures: Array[Byte]
                    ) extends Actor with Stash with utils.Scheduled with JsonStorage {

  import BlockMetaActor._

  val BlockIdFile = "block-id-mapping"

  val typeOfwMapping = new TypeToken[java.util.Map[Integer, BlockTypeId]](){}.getType

  def storeDb(factory: BlockFactory) {
    storeGson(BlockIdFile, gson.toJsonTree(factory.getWMapping, typeOfwMapping))
  }

  loadGson(BlockIdFile)
  def receive() = {
    case GsonLoaded(_, json) if json != null =>
      val defined: java.util.Map[Integer, BlockTypeId] = gson.fromJson(json, typeOfwMapping)
      val factory = BlockFactoryImpl(defined, configuredBlocks)
      storeDb(factory)
      context.become(ready(factory))
      unstashAll()
    case GsonLoaded(_, _) =>
      val map = new java.util.HashMap[Integer, BlockTypeId]()
      map.put(0, BlockTypeId.VACUUM)
      val factory = BlockFactoryImpl(map, configuredBlocks)
      storeDb(factory)
      context.become(ready(factory))
      unstashAll()
    case _ =>
      stash()
  }

  def ready(factory: BlockFactoryImpl): Receive = {
    case GetBlockFactory.MESSAGE =>
      sender ! factory
    case GetTextures =>
      sender ! Textures(textures)
  }

}

object BlockMetaActor {
  val NumTextures = 16
  val TextureSize = 16

  case class BlockClass(obstacle: Option[Boolean], shape: Option[BlockShape], state: Option[BlockState],
    durability: Option[Float], damage: Option[Float], damageMultipliers: Map[BlockOrClassId, Float],
    orientable: Option[Boolean], destroyedAs: Option[BlockTypeId], lightColour: Option[Colour],
    lightLevel: Option[LightLevel])
  case class BlockDefault(obstacle: Boolean, shape: BlockShape, state: BlockState,
    durability: Float, damage: Float, damageMultipliers: Map[BlockOrClassId, Float], orientable: Boolean,
    destroyedAs: BlockTypeId, lightColour: Colour, lightLevel: LightLevel)

  object BlockDefault {
    def apply(classes: Array[BlockClassId], classMap: Map[BlockClassId, BlockClass]): BlockDefault = {
      var obstacle = true
      var shape = BlockShape.BLOCK
      var state = BlockState.SOLID
      var durability = BlockType.DEFAULT_DURABILITY
      var damage = BlockType.DEFAULT_DAMAGE
      var damageMultipliers: Map[BlockOrClassId, Float] = Map()
      var orientable = false
      var destroyedAs = BlockTypeId.SELF
      var lightColour = Colour.WHITE
      var lightLevel = LightLevel.DARK
      for(id <- classes.reverse) {
        classMap.get(id) map { c =>
          c.obstacle.map(obstacle = _)
          c.shape.map(shape = _)
          c.state.map(state = _)
          c.durability.map(durability = _)
          c.damage.map(damage = _)
          damageMultipliers = damageMultipliers ++ c.damageMultipliers
          c.orientable.map(orientable = _)
          c.destroyedAs.map(destroyedAs = _)
          c.lightColour.map(lightColour = _)
          c.lightLevel.map(lightLevel = _)
        }
      }
      apply(obstacle, shape, state, durability, damage, damageMultipliers, orientable, destroyedAs, lightColour, lightLevel)
    }
  }

  def textureFilename(idString: String): String =
    s"/textures/$idString.png"

  def loadTexture(idString: String): BufferedImage = {
    val textureFile = getClass.getResource(textureFilename(idString))
    if(textureFile == null) throw new IllegalStateException(s"No resource for texture: ${textureFilename(idString)}")
    ImageIO.read(textureFile)
  }

  def insertTexture(i: Int, texture: BufferedImage, g: Graphics) {
    val x = (i % NumTextures) * TextureSize
    val y = (NumTextures - (i / NumTextures) - 1) * TextureSize
    g.drawImage(texture, x, y, null)
  }

  def readClasses(config: TypesafeConfig): Array[BlockClassId] = if(config.hasPath("classes")) {
    val classConfig = config.getConfig("classes")
    classConfig.root.entrySet.asScala.map( e =>
      if(e.getValue.valueType != ConfigValueType.NULL) {
        if(e.getValue.valueType == ConfigValueType.OBJECT) {
          val innerConfig = e.getValue.asInstanceOf[ConfigObject].toConfig
          if(innerConfig.hasPath("order"))
            Some((BlockClassId.fromString(e.getKey.replace("\"", "")), innerConfig.getInt("order")))
          else
            Some((BlockClassId.fromString(e.getKey.replace("\"", "")), 0))
        } else {
          Some((BlockClassId.fromString(e.getKey.replace("\"", "")), 0))
        }
      } else {
        None
      }
    ).flatten.toSeq.sortWith(_._2 < _._2).map(_._1).toArray
  } else {
    BlockType.NO_CLASSES;
  }

  def readDamageMultipliers(config: TypesafeConfig): Map[BlockOrClassId, Float] =
    if(config.hasPath("damage-multipliers")) {
      val damageConfig = config.getConfig("damage-multipliers")
      damageConfig.root.entrySet.asScala.map( e =>
        Option(damageConfig.getDouble(e.getKey)).map { multiplier =>
          (BlockOrClassId.fromString(e.getKey), multiplier.toFloat)
        }
      ).flatten.toMap
    } else {
      Map.empty[BlockOrClassId, Float]
    }



  def readIsObstacle(config: TypesafeConfig): Option[Boolean] = if(config.hasPath("obstacle")) {
    Some(config.getBoolean("obstacle"))
  } else {
    None
  }

  def readShape(config: TypesafeConfig): Option[BlockShape] = if(config.hasPath("shape")) {
    Some(BlockShape.fromString(config.getString("shape")))
  } else {
    None
  }

  def readState(config: TypesafeConfig): Option[BlockState] = if(config.hasPath("state")) {
    Some(BlockState.fromString(config.getString("state")))
  } else {
    None
  }

  def readDurability(config: TypesafeConfig): Option[Float] = if(config.hasPath("durability")) {
    Some(config.getDouble("durability").toFloat)
  } else {
    None
  }

  def readDamage(config: TypesafeConfig): Option[Float] = if(config.hasPath("damage")) {
    Some(config.getDouble("damage").toFloat)
  } else {
    None
  }

  def readOrientable(config: TypesafeConfig): Option[Boolean] = if(config.hasPath("orientable")) {
    Some(config.getBoolean("orientable"))
  } else {
    None
  }

  def readDestroyedAs(config: TypesafeConfig): Option[BlockTypeId] = if(config.hasPath("destroyed-as")) {
    Some(BlockTypeId.fromString(config.getString("destroyed-as")))
  } else {
    None
  }

  def readLightColour(config: TypesafeConfig): Option[Colour] = if(config.hasPath("light-colour")) {
    Some(Colour.fromRgbHexString(config.getString("light-colour")))
  } else {
    None
  }

  def readLightLevel(config: TypesafeConfig): Option[LightLevel] = if(config.hasPath("light-level")) {
    Some(LightLevel.get(config.getInt("light-level")))
  } else {
    None
  }

  def blockType(idString: String, config: TypesafeConfig, texturePosition: Int,
                classMap: Map[BlockClassId, BlockClass]): (BlockTypeId, BlockType) = {
    val typeId = BlockTypeId.fromString(idString)
    val classes = readClasses(config)
    val d = BlockDefault(classes, classMap)
    val isObstacle = readIsObstacle(config).getOrElse(d.obstacle)
    val shape = readShape(config).getOrElse(d.shape)
    val state = readState(config).getOrElse(d.state)
    val durability = readDurability(config).getOrElse(d.durability)
    val damage = readDamage(config).getOrElse(d.damage)
    val damageMultipliers = (for((k, v) <- (readDamageMultipliers(config) ++ d.damageMultipliers)) yield {
      k -> Float.box(v)
    }) asJava
    val orientable = readOrientable(config).getOrElse(d.orientable)
    val destroyedAs = readDestroyedAs(config).getOrElse(d.destroyedAs)
    val lightLevel = readLightLevel(config).getOrElse(d.lightLevel)
    val lightColour = readLightColour(config).getOrElse(d.lightColour)
    val blockType = if(config.hasPath("faces")) {
      val faces = config.getIntList("faces")
      if(faces.size != 6) throw new IllegalStateException("There must be exactly 6 faces")
      new BlockType(faces.asScala.map(_ + texturePosition).toArray, shape, isObstacle, false, state, classes, durability, damage, damageMultipliers, orientable, destroyedAs, lightColour, lightLevel)
    } else {
      /* Default is to assume only one texture for all faces */
      new BlockType(Array(texturePosition,texturePosition,texturePosition,texturePosition,texturePosition,texturePosition), shape, isObstacle, false, state, classes, durability, damage, damageMultipliers, orientable, destroyedAs, lightColour, lightLevel)
    }
    typeId -> blockType
  }

  def isTransparent(texture: BufferedImage): Boolean = {
    for(
      x <- 0 until texture.getWidth;
      y <- 0 until texture.getHeight) {
      if(0xFF00FF == (texture.getRGB(x, y) & 0x00FFFFFF)) return true
    }
    return false
  }

  def withTransparent(t: BlockType, transparent: Boolean): BlockType =
    new BlockType(t.getFaces, t.getBlockShape, t.isObstacle, transparent, t.getBlockState, t.getClasses, t.getDurability, t.getDamage, t.getDamageMultipliers, t.isOrientable, t.getDestroyedAs, t.getLightColour, t.getLightLevel)

  def parseClass(config: TypesafeConfig): BlockClass = {
    val isObstacle = readIsObstacle(config)
    val shape = readShape(config)
    val state = readState(config)
    val durability = readDurability(config)
    val damage = readDamage(config)
    val damageMultipliers = readDamageMultipliers(config)
    val orientable = readOrientable(config)
    val destroyedAs = readDestroyedAs(config)
    val lightColour = readLightColour(config)
    val lightLevel = readLightLevel(config)
    BlockClass(isObstacle, shape, state, durability, damage,
      damageMultipliers, orientable, destroyedAs, lightColour,
      lightLevel)
  }

  def parseClasses(config: TypesafeConfig): Map[BlockClassId, BlockClass] = {
    config.root.entrySet.asScala.map { e =>
      BlockClassId.fromString(e.getKey) -> parseClass(config.getConfig(e.getKey))
    } toMap
  }

  def parseBlocks(config: TypesafeConfig, classMap: Map[BlockClassId, BlockClass]): (Seq[(BlockTypeId, BlockType)], Array[Byte]) = {
    val blocks = config.root.entrySet.asScala.map { e =>
      e.getKey -> config.getConfig(e.getKey)
    }
    var texturePosition = 0
    val textures = new BufferedImage(
      NumTextures * TextureSize,
      NumTextures * TextureSize,
      BufferedImage.TYPE_INT_RGB)
    val texturesGraphics = textures.getGraphics()
    val blockSeq = (for((idString, block) <- blocks) yield {
      val t = blockType(idString, block, texturePosition, classMap)
      val maxIndex = t._2.getFaces().max + 1
      val numTextures = maxIndex - t._2.getFaces().min
      val img = loadTexture(idString)
      var transparent = false
      for(i <- 0 until numTextures) {
        val texture = img.getSubimage(i * TextureSize, 0, TextureSize, TextureSize)
        if(isTransparent(texture)) transparent = true
        insertTexture(texturePosition + i, texture, texturesGraphics)
      }
      texturePosition = maxIndex
      t._1 -> withTransparent(t._2, transparent)
    }) toSeq
    val texturesBinary = new ByteArrayOutputStream()

    ImageIO.write(textures, "png", texturesBinary)

    import java.io.File
    ImageIO.write(textures, "png", new File("textures.png"))
    (blockSeq, texturesBinary.toByteArray)
  }

  @PluginConstructor
  def props(
             name: String, universe: ActorRef,
             @Config(key = "json-storage") jsonStorage: ActorRef,
             @Config(key = "blocks") blockConfig: TypesafeConfig,
             @Config(key = "classes", optional = true) classConfig: TypesafeConfig
           ): Props = {
    print("Loading block data... ")
    val classMap = if(classConfig != null) {
      parseClasses(classConfig)
    } else {
      Map.empty[BlockClassId, BlockClass]
    }
    val (blocks, textures) = parseBlocks(blockConfig, classMap)
    println("done!")
    Props(
      classOf[BlockMetaActor],
      name,
      jsonStorage,
      blocks,
      textures
    )
  }
}
