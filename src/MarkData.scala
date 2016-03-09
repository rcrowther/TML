package tml



/** Carries data on a mark, for tracking and reporting errors.
  *
  * @param startPos where the mark starts, for blocks on the whitespace.
  * @param attrs the parsed or derived attributes of the block.
  */
final class MarkData(
  val startPos: Int,
  var attrs: MarkAttributes
)
{

  def isTextParagraph: Boolean = (this == MarkData.textParagraph())

  override def toString()
      : String =
  {
    val b = new StringBuilder()
    b ++= "MarkData("
    b ++= "controlStartPos:"
    b append startPos
    b ++= ", attrs:"
    b append attrs
    b += ')'
    b.result
  }

}//MarkData



object MarkData {

  // TODO: Empty would be good, for parse start?

  def inline(pos: Int, attrs: MarkAttributes)
      : MarkData =
  {
    new MarkData(pos, attrs)
  }

  def paragraph(pos: Int, attrs: MarkAttributes)
      : MarkData =
  {
    new MarkData(pos, attrs)
  }

  private val textParagraphThing = new MarkData(0, MarkAttributes())

  def textParagraph(): MarkData = textParagraphThing

  def apply(pos: Int, attrs: MarkAttributes)
      : MarkData =
  {
    new MarkData(pos, attrs)
  }

}//MarkData
