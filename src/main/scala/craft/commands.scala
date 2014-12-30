package craft.protocol

case class Chunk(p: Int, q: Int, key: Option[Int])
case class SendBlock(p: Int, q: Int, x: Int, y: Int, z: Int, w: Int)
case class You(pid: Int, p: Int, q: Int, x: Int, y: Int, z: Int)
case class Nick(pid: Int, name: String)
case class Authenticate(name: String, token: String)
case class Talk(message: String)
case class Disconnect(pid: Int)
case class Sign(x: Int, y: Int, z: Int, face: Int, text: String)
case class Version(version: Int)
case class Position(x: Float, y: Float, z: Float, rx: Float, ry: Float)
