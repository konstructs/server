package konstructs

import scala.collection.mutable

case class BlockMachine(alphabet: Map[Char, Int]) {
  import BlockMachine._

  def interpret(program: String, initPos: Position, initDir: Matrix = Upwards):
      Seq[(Position, Int)] = {

    val stack: mutable.Stack[(Position, Matrix)] = mutable.Stack()
    val blocks: mutable.ListBuffer[(Position, Int)] = mutable.ListBuffer()
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
          pos = pos + dir.adg
          blocks += ((pos, alphabet(a)))
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
}
