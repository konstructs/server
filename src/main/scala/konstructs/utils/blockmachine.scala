package konstructs.utils

import scala.collection.mutable
import scala.collection.JavaConverters._
import konstructs.Matrix
import konstructs.api._

case class BlockMachine(alphabet: Map[Char, BlockTypeId]) {
  import BlockMachine._

  def interpretJava(program: String, initPos: Position, initDir: Matrix):
      java.util.Collection[Placed[Block]] =
    interpret(program, initPos, initDir).asJavaCollection

  def interpretJava(program: String, initPos: Position):
      java.util.Collection[Placed[Block]] =
    interpretJava(program, initPos, Upwards)


  def interpret(program: String, initPos: Position, initDir: Matrix = Upwards):
      Seq[Placed[Block]] = {

    val stack: mutable.Stack[(Position, Matrix)] = mutable.Stack()
    val blocks: mutable.ListBuffer[Placed[Block]] = mutable.ListBuffer()
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
          blocks += Placed(pos, Block(None, alphabet(a)))
          pos = pos + dir.adg
      }
    }
    blocks.toSeq
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

  def fromJavaMap(alphabet: java.util.Map[java.lang.Character, BlockTypeId]) =
    apply(Map(alphabet.asScala.toSeq.map({
      case (k, v) => (k.toChar, v)
    }) :_*))
}
