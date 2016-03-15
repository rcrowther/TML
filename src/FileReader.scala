package tml

import java.nio.file.Path
import java.nio.file.Files
import java.io.File
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import scala.collection.JavaConversions._



import java.io.InputStreamReader
import java.io.FileInputStream



/** Methods for retrieving text/char data in files.
  *
  * All `apply` methods strip the lineend.
  *
  * These methods are not provided as a final solution, but for
  * assessment and testing.
  */
object FileReader {

  def stream(s: String, charset: Charset)
      : InputStreamReader =
  {
    new InputStreamReader(new FileInputStream(s), charset);
  }


  def stream(s: String)
      : InputStreamReader =
    stream(s, StandardCharsets.UTF_8)

  def apply(s: String)
      : Traversable[String] =
    apply(s, StandardCharsets.UTF_8)

  def apply(s: String, charset: Charset)
      : Traversable[String] =
  {
    apply(new File(s), charset)
  }

  def apply(f: File)
      : Traversable[String] =
    apply(f, StandardCharsets.UTF_8)

  def apply(f: File, charset: Charset)
      : Traversable[String] =
  {
    apply(f.toPath, charset)
  }

  def apply(path: Path)
      : Traversable[String] =
    apply(path, StandardCharsets.UTF_8)

  def apply(path: Path, charset: Charset)
      : Traversable[String] =
  {
    Files.readAllLines(path, charset)
  }

}//FileReader
