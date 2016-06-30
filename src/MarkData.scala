package tml



/** Carries data on a mark, for tracking and reporting errors.
  *
  * @param control the char used to identify a mark.
  * @param tagName attribute representing the name/type of the mark.
  * @param klass attributes representing the class of the mark.
  * @param params attributes attached to the mark.
  * @param pos where the mark starts, for blocks on the whitespace.
  * @param lineNum count of lines where the mark is positioned.
  * @param linePos position of the start of the line where the mark is positioned.
  */
final class MarkData(
  val control: Char,
  var tagName: String,
  var klass: Seq[String],
  var params: Seq[String],
  val pos: Int,
  val lineNum: Int,
  val linePos: Int
)
{

  def isTextParagraph: Boolean = (this == MarkData.textParagraph())

  /** The final name/type of a mark.
    *
    * The mark may have the tag stated explicitly.  If a mark has a
    * repeated character, then the name is derived from the defaults
    * listed in the parser (depending on the control character).
    *
    * However the tag name is decided, this field carries the result.
    */
  var resolvedTagname: String = ""


  /** Sets the resolved name from a map of controls to defaults.
    *
    * If the tagname is only one char long, and matches the control,
    * then the default is used, otherwise the tag name is used.
    *
    * @param default tagname to use if the mark is defaulted.
    */
  def tagNameResolve(default: String)
  {
    // NB: The zero test is wasted on block opening marks
    // but no big deal.
    resolvedTagname =
      if (tagName.size == 0 || (tagName.size == 1 && tagName(0) == control)) {
        default
      }
      else tagName
  }


  /** Sets the resolved name from a map of controls to defaults.
    *
    * If the tagname is only one char long, and matches the control,
    * then the default is used, otherwise the tag name is used.
    *
    * @param map a map of chars to defaults.
    */
  def tagNameResolve(map: Map[Char, String])
  {
    // NB: The zero test is wasted on block opening marks
    // but no big deal.
    resolvedTagname =
      if (tagName.size == 0 || (tagName.size == 1 && tagName(0) == control)) {
        map(control)
      }
      else tagName
  }

  /** Number of times the control appears at the start of the tag.
    *
    * Used to count multiple control repetitions e.g. for headline
    * controls.
    */
/*
  def tagControlPrefixCount
      : Int =
  {
    var i = 0
    var cont = true

    while ((i < tagName.size) && (control == tagName(i))) {
      i += 1
    }
    i
  }
*/
  /** Split the tagname between controls and text.
    *
    * This method separates controls at the start of the tagname from
    * any text at the end.
    * 
    * Either side may return empty.
    */
/*
  def splitTagControls()
      : (String, String) =
  {
    tagName.splitAt(tagControlPrefixCount)
  }
*/


  /** Appends a represention of the original mark from markData.
    */
 def addMarkString(b: StringBuilder)
: StringBuilder =
{
      b += '\''
      b append control
      b append tagName
      b += '\''
b
}

  /** Creates a represention of the original mark from markData.
    */
 def toMarkString()
: String =
{
addMarkString(new StringBuilder()).result()
}


  override def toString()
      : String =
  {
    val b = new StringBuilder()
    b ++= "MarkData("
    b ++= "control:"
    b append control
    b ++= ", tagName:"
    b append tagName
    b ++= ", klass:"
    b append klass
    b ++= ", params:"
    b append params
    b ++= ", resolvedTagname:"
    b append resolvedTagname
    b ++= " pos:"
    b append pos
    b ++= " lineNum:"
    b append lineNum
    b ++= " linePos:"
    b append linePos
    b += ')'
    b.result
  }

}//MarkData



object MarkData {

  private val textParagraphThing = MarkData.apply()

  def textParagraph(): MarkData = textParagraphThing

  def apply(
    control: Char,
    it: InputIterator
  )
      : MarkData =
  {
    new MarkData(
      control,
      "",
      Seq.empty[String],
      Seq.empty[String],
      it.pos,
      it.lineCount,
      it.lastNewlinePos
    )
  }


  def apply()
      : MarkData =
  {
    new MarkData('\u0000', "", Seq.empty[String], Seq.empty[String], 0, 0, 0)
  }

  def apply(
    control: Char,
    tagName: String,
    klass: Seq[String],
    params: Seq[String],
    pos: Int,
    lineNum: Int,
    linePos: Int
  )
      : MarkData =
  {
    new MarkData(control, tagName, klass, params, pos, lineNum, linePos)
  }

}//MarkData
