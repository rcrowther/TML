package tml

import java.nio.file.Path
import java.nio.file.Files
import java.io.File
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets
import scala.collection.JavaConversions._

object FileReader {

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
