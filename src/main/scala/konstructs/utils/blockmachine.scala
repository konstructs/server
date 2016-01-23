package konstructs.utils

import scala.collection.mutable
import scala.collection.JavaConverters._
import konstructs.Matrix
import konstructs.api._

case class BlockMachine(alphabet: Map[Char, BlockTypeId]) {
  import BlockMachine._

  def interpretJava(program: String, initPos: Position, initDir: Matrix):
      java.util.Map[Position, BlockTypeId] =
    interpret(program, initPos, initDir).asJava

  def interpretJava(program: String, initPos: Position):
      java.util.Map[Position, BlockTypeId] =
    interpretJava(program, initPos, Upwards)


  def interpret(program: String, initPos: Position, initDir: Matrix = Upwards):
      mutable.Map[Position, BlockTypeId] = {

    val stack: mutable.Stack[(Position, Matrix)] = mutable.Stack()
    val blocks: mutable.Map[Position, BlockTypeId] = mutable.HashMap()
    var pos = initPos
    var dir = initDir

    for(i <- program) {
      i match {
        case '&' =>
          dir = dir * Down
        case '^' =>
          dir = dir * Up
        case '+' =>
          dir = dir * Left
        case '-' =>
          dir = dir * Right
        case '\\' =>
          dir = dir * RollLeft
        case '/' =>
          dir = dir * RollRight
        case '[' =>
          stack.push((pos, dir))
        case ']' =>
          val (oldPos, oldDir) = stack.pop
          pos = oldPos
          dir = oldDir
        case a =>
          blocks += pos -> alphabet.getOrElse(a, BlockTypeId.Vacuum)
          pos = pos + dir.adg
      }
    }
    blocks
  }

}

object BlockMachine {
  val Upwards = Matrix(0,  1,  0,
                       1,  0,  0,
                       0,  0,  1)
  val Left = Matrix(0,  1,  0,
                    -1, 0,  0,
                    0,  0,  1)
  val Right = Matrix(0, -1,  0,
                     1,  0,  0,
                     0,  0,  1)
  val Down = Matrix(0,  0, -1,
                    0,  1,  0,
                    1,  0,  0)
  val Up = Matrix(0,  0,  1,
                  0,  1,  0,
                  -1, 0,  0)
  val RollLeft = Matrix(1,  0,  0,
                        0,  0, -1,
                        0,  1,  0)
  val RollRight = Matrix(1,  0,  0,
                         0,  0,  1,
                         0, -1,  0)

  val VacuumMachine = BlockMachine(Map())
  def vacuumMachine = VacuumMachine
  def fromJavaMap(alphabet: java.util.Map[java.lang.Character, BlockTypeId]) =
    apply(Map(alphabet.asScala.toSeq.map({
      case (k, v) => (k.toChar, v)
    }) :_*))
}
