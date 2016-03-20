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

  /** Empties the output.
    *
    * May discard or flush results. Will leavve the output ready to be used again.
    */
  def clear()

}//OutputTarget


/** Provides methods for creating output targets.
  *
  * Some output needs care with encoding. Output from TML is UTF-16.
  * This is ok for general Java methods, but may produce mangled
  * output if converted to bytes, or pushed to the outside world.
  *
  * Note that any handlin of the supplied object, such as sttream closing or 
  * results from StringBuilders, must be handled externall to the parser. 
  */
object OutputTarget {

  /** The output target which produces no values.
    */
  val empty: OutputTarget = new OutputTarget {
    def +=(c: Char) {}
    def ++=(s: String) {}
    def clear() {}
  }

  /** Creates an output target to stdout.
    */
  def apply(): OutputTarget = new OutputTarget{
    def +=(c: Char) = print(c)
    def ++=(s: String) = print(s)
    def clear() { println }
  }

  /** Creates an output target using a string builder.
    *
    * The builder is available without results.
    */
  def apply(b: StringBuilder): OutputTarget = new OutputTarget{
    //private val b = new StringBuilder()
    def +=(c: Char) = { b += c }
    def ++=(s: String) = { b ++= s }
    def clear() { b.clear() }
  }

  /*
   def close(): Option[Exception] = {
   try {
   os.close()
   None
   }
   catch {
   case e: Exception => Some(e)
   }
   }
   */


  /** Creates an output target from a stream.
    *
    * Note that output from TML is UTF-16.
    */
  def apply(os: OutputStream): OutputTarget = new OutputTarget{
    def +=(c: Char) = { os.write(c) }
    def ++=(s: String) = { os.write(s.getBytes(), 0, s.size) }
    def clear() = os.flush()
  }


  /** Creates an output target from a stream, in a specified encoding.
    *
    * For the conversion, wraps in an [[java.io.OutputStreamWriter]].
    */
  def apply(os: OutputStream, cs: Charset): OutputTarget = new OutputTarget{
    val osw = new OutputStreamWriter(os, cs)
    def +=(c: Char) = { osw.write(c) }
    def ++=(s: String) = { osw.write(s, 0, s.size) }
    def clear() = osw.flush()
  }

  /** Creates an output target from a stream, encoded as utf8.
    *
    * The stream of data is converted to UTF-8.
    *
    * For the conversion, wraps in an [[java.io.OutputStreamWriter]].
    */
  def toUF8Stream(os: OutputStream)
      : OutputTarget =
  {
    apply(os: OutputStream, StandardCharsets.UTF_8)
  }

  implicit def outputTargetFromStringBuilder(b: StringBuilder): OutputTarget = apply(b)
  implicit def outputTargetFromStream(os: OutputStream): OutputTarget = apply(os)

}//OutputTarget
