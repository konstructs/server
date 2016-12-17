package konstructs.utils

import java.util.zip.{Inflater, Deflater}

package object compress {

  def deflate(data: Array[Byte], buffer: Array[Byte], offset: Int): Int = {
    val compresser = new Deflater()
    compresser.setInput(data)
    compresser.finish()
    val size = compresser.deflate(buffer, offset, buffer.size - offset)
    compresser.end()
    size + offset
  }

  def inflate(data: Array[Byte], buffer: Array[Byte], offset: Int, length: Int): Int = {
    val decompresser = new Inflater()
    decompresser.setInput(data, offset, length)
    val size = decompresser.inflate(buffer)
    decompresser.end()
    size
  }

}
