package craft

import java.util.zip.{ Inflater, Deflater }

package object compress {

  def deflate(data: Array[Byte], buffer: Array[Byte]): Array[Byte] = {
    val compresser = new Deflater()
    compresser.setInput(data)
    compresser.finish()
    val size = compresser.deflate(buffer)
    compresser.end()
    buffer.slice(0, size)
  }

  def inflate(data: Array[Byte], buffer: Array[Byte]): Int = {
    val decompresser = new Inflater()
    decompresser.setInput(data, 0, data.size)
    val size = decompresser.inflate(buffer)
    decompresser.end()
    size
  }

}
