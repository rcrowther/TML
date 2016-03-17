package tml



/** Holds details on errors.
  * 
  * The positions should be the point of failure (not a guess at
  * syntax). The positions and the messsge are mandatory, other fields
  * are optional.
  *
  * @param lineNum the line number of the error.
  * @param lineStartPos position of the start of the line.
  * @param pos the position of the error. This is the
  *  absolute position of the error in the file.
  * @param markData data attached to the error. May 
  * not be in the error position (e.g tail errors hold data
  * on the open mark).
  * @param message a message describing the error.
  */
final class Error(
  val lineNum: Int,
  val lineStartPos: Int,
  val pos: Int,
  val openingMarkData: Option[MarkData],
  val message: String,
  val advice: String
)
{

  /** Returns a string representing the position of this error.
    */
  def addPosString(b: StringBuilder)
  {
    b append lineNum
    b += ':'
    b append pos - lineStartPos
  }

}//Error



object Error {

  def apply(
    it: InputIterator,
    message: String
  )
      : Error =
  {
    new Error(
      it.lineCount,
      it.lastNewlinePos,
      it.pos,
      None,
      message,
      ""
    )
  }

  def apply(
    it: InputIterator,
    message: String,
    advice: String
  )
      : Error =
  {
    new Error(
      it.lineCount,
      it.lastNewlinePos,
      it.pos,
      None,
      message,
      advice
    )
  }

  def apply(
    it: InputIterator,
    openingMarkData: MarkData,
    message: String,
    advice: String
  )
      : Error =
  {
    new Error(
      it.lineCount,
      it.lastNewlinePos,
      it.pos,
      Some(openingMarkData),
      message,
      advice
    )
  }

  /** Sets the primary position from a mark, not an iterator.
    */
  def positionByMarkdata(
    markData: MarkData,
    message: String,
    advice: String
  )
      : Error =
  {
    new Error(
      markData.pos,
      markData.lineNum,
      markData.linePos,
      None,
      message,
      advice
    )
  }

}//Error
