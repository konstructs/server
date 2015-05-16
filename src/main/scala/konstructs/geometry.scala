package konstructs

case class Matrix(a: Int, b: Int, c: Int, d: Int, e: Int, f: Int, g: Int, h: Int, i: Int) {
  def *(m: Matrix) =
    Matrix(a*m.a + b*m.d + c*m.g, a*m.b + b*m.e + c*m.h, a*m.c + b*m.f + c*m.i,
           d*m.a + e*m.d + f*m.g, d*m.b + e*m.e + f*m.h, d*m.c + e*m.f + f*m.i,
           g*m.a + h*m.d + i*m.g, g*m.b + h*m.e + i*m.h, g*m.c + h*m.f + i*m.i)

  def *(p: Position) = Position(a*p.x + b*p.y + c*p.z,
                                d*p.x + e*p.y + f*p.z,
                                g*p.x + h*p.y + i*p.z)
  def adg = Position(a, d, g)
  def beh = Position(b, e, h)
  def cfi = Position(c, f, i)
  override def toString = s"Matrix(\n\t$a,\t$b,\t$c\n\t$d,\t$e,\t$f\n\t$g,\t$h,\t$i\n)"
}

case class Position(x: Int, y: Int, z: Int) {
  def +(p: Position) = Position(x + p.x, y + p.y, z + p.z)
}

object Position {
  def apply(pos: protocol.Position): Position =
    apply(math.round(pos.x), math.round(pos.y), math.round(pos.z))
}
