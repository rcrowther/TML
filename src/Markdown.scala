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
class Markdown
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
      b ++= " class=\""
      b ++= md.klass
      b ++= "\""
    }
    if (md.text != "") {
      b ++= " title=\""
      b ++= md.text
      b ++= "\""
    }
    if (md.url != "") {
      b ++= " href=\""
      b ++= md.url
      b ++= "\""
    }
  }

  // div, main, section, article, aside
  // ol, ul, dl,
  // li, dd, dt, blockquote
  def renderBlockOpen(md: MarkData)
 {
    b += '<'
    b ++= md.resolvedTagname
    attributesStockRender(md)
    b += '>'
  }

  def renderBlockClose(
    md: MarkData
  )
  {
    b ++= "<\\"
    b ++= md.resolvedTagname
    b += '>'
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

    b += '<'
    b ++= md.resolvedTagname

    if (md.klass != "") {
      b ++= " class=\""
      b ++= md.klass
      b ++= "\""
    }
    if (md.text != "") {
      b ++= " title=\""
      b ++= md.text
      b ++= "\""
    }
    if (md.url != "") {
      b ++= " href=\""
      b ++= md.url
      b ++= "\""
    }
    b += '>'

    //println(s"renderOpen... '${b.result}'")
  }

  def renderParagraphClose(md: MarkData)
 {
    b ++= "<\\"
    b ++= md.resolvedTagname
    b += '>'
  }

  // used for a, i, b, span
  def renderInlineOpen(md: MarkData)
{

    // Markdown link
    // link = [an example](http://example.com/ "Title")
    if (md.resolvedTagname == "a") {
      b += '['
    }
    else {
      if (md.resolvedTagname == "em") {
        b += '*'
      }
      else {
        // catch literal, ignore
        if (md.resolvedTagname != InlineLiteralTagname) {
          b += '<'
          b ++= md.resolvedTagname
          attributesStockRender(md)
          b += '>'
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
      b ++= "]("
      if (md.url != "") {
        b ++= md.url
      }

      if (md.text != "") {
        b += '\"'
        b ++= md.text
        b += '\"'
      }
      b += ')'
    }
    else {
      if (md.resolvedTagname == "em") {
        b += '*'
      }
      else {
        // catch literal, ignore
        if (name != InlineLiteralTagname) {
          b ++= "<\\"
          b ++= name
          b += '>'
        }
      }
    }
  }

  // Used for img
  def renderInlineSelfClosingMark(md: MarkData)
 {

    // Markdown img
    //img = ![Alt text](/path/to/img.jpg "Optional title")
    b += '!'

    if (md.text != "") {
      b += '['
      b ++= md.text
      b += ']'
    }

    if (md.url != "") {
      b += '('
      b ++= md.url
      b += ')'
    }

    //println(s"render... '${b.result}'")
  }

  // used for hr
  def renderBlockSelfClosingMark(md: MarkData) 
{
    // Markdown hr
    b ++= "* * *"
  }

  def renderTextParagraphOpen() 
{
    // put the newline back
    b += '\n'
  }

  def renderTextParagraphClose() 
{
    // put the newline back
    b += '\n'
  }

}//Markdown
