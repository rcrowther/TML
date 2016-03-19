package tml


import java.io.{OutputStreamWriter, OutputStream}
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

import scala.language.implicitConversions


//tml.HTML2(OutputTarget(FileReader("/home/rob/Code/scala/TML/text/SPEC")))


/** Targets for output.
  *
  */
trait OutputTarget
    extends Definitions
{

  /** Writes a char to putput.
    */
  def +=(c: Char)

  /** Writes a string to putput.
    */
  def ++=(s: String)

}//OutputTarget


/** Provides methods for creating output targets.
  *
  * Some output needs care with encoding. Output from TML is UTF-16.
  * This is ok for general Java methods, but may produce mangled
  * output if converted to bytes, or pushed to the outside world.
  */
object OutputTarget {

  /** The output target which produces no values.
    */
  val empty: OutputTarget = new OutputTarget {
    def +=(c: Char) {}
    def ++=(s: String) {}
  }

  /** Creates an output target to stdout.
    */
  def printer(): OutputTarget = new OutputTarget{
    def +=(c: Char) = print(c)
    def ++=(s: String) = print(s)
  }

  /** Creates an output target using a string builder.
    *
    * The builder is available without results.
    */
  def stringBuilder(): OutputTarget = new OutputTarget{
    private val b = new StringBuilder()
    def +=(c: Char) = { b += c }
    def ++=(s: String) = { b ++= s }
    def result() : String = b.result()
    def builder : StringBuilder = b
  }

  /*
   /** Creates an output target from a stream.
   */
   def outputStream(os: OutputStream, cs: Charset): OutputTarget = new OutputTarget{
   val osw = new OutputStreamWriter(os, cs)
   def +=(c: Char) = { osw.write(c) }
   def ++=(s: String) = { osw.write(s, 0, s.size) }

   def close(): Option[Exception] = {
   try {
   osw.close()
   None
   }
   catch {
   case e: Exception => Some(e)
   }
   }
   }

   /** Creates an output target from a stream, encoded as utf8.
   */
   def outputStream(os: OutputStream): OutputTarget = outputStream(os: OutputStream, StandardCharsets.UTF_8)
   */

  /** Creates an output target from a stream.
    *
    * Note that output from TML is UTF-16.
    */
  //NB: currentky, not our business to state encoung, etc. R.C.
  def outputStream(os: OutputStream): OutputTarget = new OutputTarget{
    def +=(c: Char) = { os.write(c) }
    def ++=(s: String) = { os.write(s.getBytes(), 0, s.size) }

    def close(): Option[Exception] = {
      try {
        os.close()
        None
      }
      catch {
        case e: Exception => Some(e)
      }
    }
  }

  /** Creates an output target from a stream.
    *
    * Wraps in an [[java.io.OutputStreamWriter]] to convert.
    */
  def toUF8Stream(os: OutputStream): OutputTarget = new OutputTarget{
    val osw = new OutputStreamWriter(os, StandardCharsets.UTF_8)
    def +=(c: Char) = { osw.write(c) }
    def ++=(s: String) = { osw.write(s, 0, s.size) }

    def close(): Option[Exception] = {
      try {
        osw.close()
        None
      }
      catch {
        case e: Exception => Some(e)
      }
    }
  }

  implicit def outputTargetFromStream(os: OutputStream): OutputTarget = outputStream(os)

}//OutputTarget
