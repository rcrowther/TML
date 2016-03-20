package tml



/** Attempt translation of TML to Markdown.
  *
  * One way to do this is to convert to HTML then pass in. Markdown
  * should pass HTML through, untouched.
  *
  * This class will attempt to make Markdown marks from TML. THis is a
  * more idomatic solution than HTML, and easier to edit. Markdown is
  * more expressive than TML, so the results will be limited.
  *
  * It is best to restrain markup to common TML idoms. Markdown has
  * little of the flexibility of TML, and TML guesses what is intended
  * from tagnames. Override tagnames, and the result may be caotic.
  *
  * [[https://daringfireball.net/projects/markdown]]
  */
// TODO: Escapes, liss, blockquote, headline
//TODO: Perhaps should override HTML
class Markdown(val ot: OutputTarget)
    extends Parser
{

  val blockBracketedTagnameAliases = Map(
    "d" -> "div",
    "c" -> "code",
    "bq" -> "blockquote"
  )

  val BlockBracketedMarks = Map(
    '#' -> "section",
    '+' -> "ul",
    '-' -> "li",
    '?' -> "pre"
  )

  val BlockParagraphDefaultedMarks: Map[Char, String] = Map(
    '~' -> "dd",
    ':' -> "dt",
    '@' -> "li"
  )

  override val BlockBracketedLiteralMark: Char = '?'

  val BlockParagraphNoDefaultMarks: Seq[Char] = Seq('=')

  // Could be used for 'a'
  // but span is generic so intuitive
  val InlineMarkDefault: String = "span"

  val inlineBracketedTagnameAliases = Map(
    "s" -> "span",
    "bold" -> "b",
    "italic" -> "i"
  )

  // Only used for hr
  override val BlockSelfClosingMark: Char = '_'
  val BlockSelfClosingMarkDefault = "hr"

  // Only used for img
  override val InlineSelfClosingMark: Char = '*'
  val InlineSelfClosingMarkDefault: String = "img"

  // Renderers
  /** html rendering of non-tag attributes.
    *
    *  Double quoted and mapped,
    *
    */
  private def attributesStockRender(md: MarkData)
 {

    if (md.klass != "") {
      ot ++= " class=\""
      ot ++= md.klass
      ot ++= "\""
    }
    if (md.params.isDefinedAt(0) && md.params(0) != "") {
      ot ++= " alt=\""
      ot ++= md.params(0)
      ot ++= "\""
    }
    if (md.params.isDefinedAt(1) && md.params(1) != "") {
      ot ++= " src=\""
      ot ++= md.params(1)
      ot ++= "\""
    }
  }

  // div, main, section, article, aside
  // ol, ul, dl,
  // li, dd, dt, blockquote
  def renderBlockOpen(md: MarkData)
 {
    ot += '<'
    ot ++= md.resolvedTagname
    attributesStockRender(md)
    ot += '>'
  }

  def renderBlockClose(
    md: MarkData
  )
  {
    ot ++= "<\\"
    ot ++= md.resolvedTagname
    ot += '>'
  }

  // used for h?, pre
  def renderParagraphOpen(md: MarkData)
{
    // fix the headline tag name
    // headlines are a little complex.
    // If there is no prefixed control char in the name,
    // then the tag name was given or defaulted, so do nothing.
    // If prefixes exist, they are counted to form the
    // resolvedTagnamename
    if (md.control == '=') {
      val (ctrls, tag) = md.splitTagControls
      md.resolvedTagname =
        if (tag.size > 0) tag
        else "h" + (ctrls.size + 1)
    }

    ot += '<'
    ot ++= md.resolvedTagname

    if (md.klass != "") {
      ot ++= " class=\""
      ot ++= md.klass
      ot ++= "\""
    }
    if (md.params.isDefinedAt(0) && md.params(0) != "") {
      ot ++= " title=\""
      ot ++= md.params(0)
      ot ++= "\""
    }
    if (md.params.isDefinedAt(1) && md.params(1) != "") {
      ot ++= " href=\""
      ot ++= md.params(1)
      ot ++= "\""
    }
    ot += '>'

    //println(s"renderOpen... '${b.result}'")
  }

  def renderParagraphClose(md: MarkData)
 {
    ot ++= "<\\"
    ot ++= md.resolvedTagname
    ot += '>'
  }

  // used for a, i, b, span
  def renderInlineOpen(md: MarkData)
{

    // Markdown link
    // link = [an example](http://example.com/ "Title")
    if (md.resolvedTagname == "a") {
      ot += '['
    }
    else {
      if (md.resolvedTagname == "em") {
        ot += '*'
      }
      else {
        // catch literal, ignore
        if (md.resolvedTagname != InlineLiteralTagname) {
          ot += '<'
          ot ++= md.resolvedTagname
          attributesStockRender(md)
          ot += '>'
        }
      }
    }
    //println(s"renderOpen... '${b.result}'")
  }


  def renderInlineClose(
    md: MarkData
  )
  {

    val name  = md.resolvedTagname

    // Markdown link
    // link = [an example](http://example.com/ "Title")
    if (name == "a") {
      ot ++= "]("
    if (md.params.isDefinedAt(1) && md.params(1) != "") {
        ot ++= md.params(1)
      }

    if (md.params.isDefinedAt(0) && md.params(0) != "") {
        ot += '\"'
      ot ++= md.params(0)
        ot += '\"'
      }
      ot += ')'
    }
    else {
      if (md.resolvedTagname == "em") {
        ot += '*'
      }
      else {
        // catch literal, ignore
        if (name != InlineLiteralTagname) {
          ot ++= "<\\"
          ot ++= name
          ot += '>'
        }
      }
    }
  }

  // Used for img
  def renderInlineSelfClosingMark(md: MarkData)
 {

    // Markdown img
    //img = ![Alt text](/path/to/img.jpg "Optional title")
    ot += '!'

    if (md.params.isDefinedAt(0) && md.params(0) != "") {
      ot += '['
      ot ++= md.params(0)
      ot += ']'
    }
    if (md.params.isDefinedAt(1) && md.params(1) != "") {
      ot += '('
      ot ++= md.params(1)
      ot += ')'
    }
    //println(s"render... '${b.result}'")
  }

  // used for hr
  def renderBlockSelfClosingMark(md: MarkData) 
{
    // Markdown hr
    ot ++= "* * *"
  }

  def renderTextParagraphOpen() 
{
    // put the newline back
    ot += '\n'
  }

  def renderTextParagraphClose() 
{
    // put the newline back
    ot += '\n'
  }

}//Markdown
