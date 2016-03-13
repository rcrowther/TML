package tml


import java.io.InputStreamReader


//tml.HTML2(InputIterator(FileReader("/home/rob/Code/scala/TML/text/SPEC")))


/** Iterator for input
  *
  * Deliberately simple. The iterator is one-time-only, only `next`
  * needs to be called.
  *
  * No tests for overrunning - the calling code must respond to `LF` +
  * `EOF`
  */
trait InputIterator
{
  // ASCII control code - ETX (end of text)
  val EOF: Char = 3
  val LF: Char = 10

  /** Count of the characters iterated.
    */
  var pos: Int = 0

  /** Get the next char.
    *
    * *Must* guarentee returning `LF` + `EOF` as the last two
    * items.
    */
  def next(): Char

  /** Look at the next character but do not advance.
    *
    * The method must return the final LF, but not the
    * EOF.
    * 
    * Unprotected if called out of source range. 
    */
  def lookForward: Char
}


/** Provides methods for creating iterators from various input types.
  *
  * All returned iterators avoid copying the string.  They also must
  * ensure returning a terminal LF + EOF.  To do this, they must test
  * characters for exhaustion.  For this reason, in some
  * circumastances, the StringBuilder and InputStream iterators
  * (taking advantage of existing underlying code) may be faster.
  */
object InputIterator {

  /** The iterator which produces no values.
    */
  val empty: InputIterator = new InputIterator {
    def next(): Char = throw new NoSuchElementException("next on empty iterator")
    def lookForward: Char = throw new NoSuchElementException("lookForward on empty iterator")
  }

  /** Creates an iterator from a string.
    */
  def apply(s: String) = new InputIterator {
    private val sz = s.size
    def next() : Char =
      if (pos < sz) { val r = s(pos); pos += 1; r }
      else {
        if (pos == sz) { pos += 1; LF }
        else EOF
      }
    def lookForward : Char =
      if (pos < sz) s(pos)
      else LF
  }

  /** Creates an iterator from a string builder.
    */
  def apply(b: StringBuilder) = new InputIterator {
    b += LF
    b += EOF
    private val s = b.result()
    private val sz = s.size - 1
    def next() : Char = { val r = s(pos); pos += 1; r }
    def lookForward: Char =  s(pos)
  }


  /** Creates an iterator from given strings.
    *
    * The iterater, in common with other iterators here, iterates the
    * chars contained in the strings (not across the strings).
    *
    * This method will not correctly parse a collection of strings
    * representing lines with lineends stripped. If lineends should be
    * appended to the given strings, see the multi-string `line`
    * method.
    */
  def apply(ts: Traversable[String]) = new InputIterator {
    private var currPos = 0
    private var currString = ""
    private var currSize = 0
    private var deliveredLF = false
    private val strSeq = ts.toSeq
    private var i = 0
    private val strCount = strSeq.size

    private def nextString() : Boolean = {
      currPos = 0
      val strNotExhausted = (i < strCount)
      if (strNotExhausted) {
        currString = strSeq(i)
        i += 1
        currSize = currString.size
      }
      else { currString = "" ; currSize = 0 }
      strNotExhausted
    }

    def next() : Char =
      if (currPos < currSize) {
        val r = currString(currPos)
        currPos += 1
        // update overall position
        pos += 1
        r
      }
      else {
        if (nextString()) next()
        else {
          // still use currPos as step
          // for two virtual end chars
          // also, update overall position
          if (!deliveredLF) { pos += 1; deliveredLF = true; LF }
          else EOF
        }
      }
    

    def lookForward : Char =
      if ((currPos) < currSize) currString(currPos)
      else {
        var nextStrIdx = i
        while (nextStrIdx < strCount && strSeq(nextStrIdx).isEmpty) {
          nextStrIdx += 1
        }
        // may look onto final LF...
        if (nextStrIdx < strCount)  strSeq(nextStrIdx)(0)
        else LF
      }
  }


  // val i = InputIterator("I", "my", "name")


  /** Creates an iterator from given strings.
    *
    * Appends a linend to every string.
    *
    * Much Java code, if it reads by line from a file, strips line
    * ends. This iterator replaces linends (which are significant in
    * TML). If parsing string lines with linends appended, the
    * iterator will generate output correctly from TML markup (at a
    * small cost of an extra char parse per line).
    *
    * The iterater, in common with other iterators here, iterates the
    * chars contained in the strings (not across the strings).
    *
    * If lineends should not be appended to the given strings, see the
    * multi-string `apply` method.
    */
  def line(ts: TraversableOnce[String]) = new InputIterator {
    private var currPos = 0
    private var currString = ""
    private var currSize = 0
    private var deliveredLF = false
    private val strSeq = ts.toSeq
    private var i = 0
    private val strCount = strSeq.size

    private def nextString() : Boolean = {
      currPos = 0
      deliveredLF = false
      val strNotExhausted = (i < strCount)
      if (strNotExhausted) {
        currString = strSeq(i)
        i += 1
        currSize = currString.size
      }
      else { currString = "" ; currSize = 0 }
      strNotExhausted
    }

    def next() : Char =
      if (currPos < currSize) {
        val r = currString(currPos)
        currPos += 1
        // update overall position
        pos += 1
        r
      }
      else {
        if (!deliveredLF) { pos += 1; deliveredLF = true; LF}
        else {
          if (nextString()) next()
          else {
            // still use currPos as step
            // for two virtual end chars
            // also, update overall position
            EOF
          }
        }
      }
    

    def lookForward : Char =
      if (currPos < currSize) currString(currPos)
      else {
        if (!deliveredLF) LF
        else {
          var nextStrIdx = i

          // may look onto final LF...
          if (nextStrIdx < strCount && !strSeq(nextStrIdx).isEmpty) {
            strSeq(nextStrIdx)(0)
          }
          else LF
        }
      }
  }


  /** Creates an iterator from an input stream.
    *
    * When the stream is created, codings may be specified.
    */
  def apply(s: InputStreamReader) = new InputIterator {

    private var frwd: Int = s.read()
    private var deliveredLF = false

    def next() : Char =
      if (frwd != -1) {
        val r = frwd
        frwd = s.read()
        pos += 1
        r.toChar
      }
      else {
        if (!deliveredLF) { pos += 1; deliveredLF = true; LF }
        else EOF
      }

    def lookForward : Char =
      if (frwd != -1) frwd.toChar else LF
  }

implicit def string2InputIterator(s: String) = InputIterator(s)
implicit def stringBuilder2InputIterator(b: StringBuilder) = InputIterator(b)
implicit def stringElems2InputIterator(elems: String *) = InputIterator(elems)
implicit def string2InputIterator(s: InputStreamReader) = InputIterator(s)

}//InputIterator

