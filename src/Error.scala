package tml



/** Holds details on errors.
  * 
  * @param lineNum the line number of the error.
  * @param linePos position of the start of the line.
  * @param pos the position of the error. This is the
  *  absolute position of the error in the file.
  * @param message a message describing the error.
  */
final class Error(
  val lineNum: Int,
  val linePos: Int,
  val pos: Int,
  val message: String
)
{

  /** Position of this error relative to the line.
    */
  def lineRelativePos: Int = pos - linePos
}
