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
  private def attributesStockRender(attrs: MarkAttributes) {
    if (attrs.id != "") {
      b ++= " id=\""
      b ++= attrs.id
      b ++= "\""
    }
    if (attrs.klass != "") {
      b ++= " class=\""
      b ++= attrs.klass
      b ++= "\""
    }
    if (attrs.text != "") {
      b ++= " title=\""
      b ++= attrs.text
      b ++= "\""
    }
    if (attrs.url != "") {
      b ++= " href=\""
      b ++= attrs.url
      b ++= "\""
    }
  }

  // div, main, section, article, aside
  // ol, ul, dl,
  // li, dd, dt, blockquote
  def renderBlockOpen(attrs: MarkAttributes) = {
    b += '<'
    b ++= attrs.resolvedTagname
    attributesStockRender(attrs)
    b += '>'
  }

  def renderBlockClose(
    attrs: MarkAttributes
  )
  {
    b ++= "<\\"
    b ++= attrs.resolvedTagname
    b += '>'
  }

  // used for h?, pre
  def renderParagraphOpen(attrs: MarkAttributes) = {
    // fix the headline tag name
    // headlines are a little complex.
    // If there is no prefixed control char in the name,
    // then the tag name was given or defaulted, so do nothing.
    // If prefixes exist, they are counted to form the
    // resolvedTagnamename
    if (attrs.control == '=') {
      val (ctrls, tag) = attrs.splitTagControls
      attrs.resolvedTagname =
        if (tag.size > 0) tag
        else "h" + (ctrls.size + 1)
    }

    b += '<'
    b ++= attrs.resolvedTagname
    if (attrs.id != "") {
      b ++= " id=\""
      b ++= attrs.id
      b ++= "\""
    }
    if (attrs.klass != "") {
      b ++= " class=\""
      b ++= attrs.klass
      b ++= "\""
    }
    if (attrs.text != "") {
      b ++= " title=\""
      b ++= attrs.text
      b ++= "\""
    }
    if (attrs.url != "") {
      b ++= " href=\""
      b ++= attrs.url
      b ++= "\""
    }
    b += '>'

    //println(s"renderOpen... '${b.result}'")
  }

  def renderParagraphClose(name: String) {
    b ++= "<\\"
    b ++= name
    b += '>'
  }

  // used for a, i, b, span
  def renderInlineOpen(attrs: MarkAttributes) = {

    // Markdown link
    // link = [an example](http://example.com/ "Title")
    if (attrs.resolvedTagname == "a") {
      b += '['
    }
    else {
      if (attrs.resolvedTagname == "em") {
        b += '*'
      }
      else {
        // catch literal, ignore
        if (attrs.resolvedTagname != InlineLiteralTagname) {
          b += '<'
          b ++= attrs.resolvedTagname
          attributesStockRender(attrs)
          b += '>'
        }
      }
    }
    //println(s"renderOpen... '${b.result}'")
  }


  def renderInlineClose(
    attrs: MarkAttributes
  )
  {

    val name  = attrs.resolvedTagname

    // Markdown link
    // link = [an example](http://example.com/ "Title")
    if (name == "a") {
      b ++= "]("
      if (attrs.url != "") {
        b ++= attrs.url
      }

      if (attrs.text != "") {
        b += '\"'
        b ++= attrs.text
        b += '\"'
      }
      b += ')'
    }
    else {
      if (attrs.resolvedTagname == "em") {
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
  def renderInlineSelfClosingMark(attrs: MarkAttributes) {

    // Markdown img
    //img = ![Alt text](/path/to/img.jpg "Optional title")
    b += '!'

    if (attrs.text != "") {
      b += '['
      b ++= attrs.text
      b += ']'
    }

    if (attrs.url != "") {
      b += '('
      b ++= attrs.url
      b += ')'
    }

    //println(s"render... '${b.result}'")
  }

  // used for hr
  def renderBlockSelfClosingMark(attrs: MarkAttributes) {
    // Markdown hr
    b ++= "* * *"
  }

  def renderTextParagraphOpen() {
    // put the newline back
    b += '\n'
  }

  def renderTextParagraphClose() {
    // put the newline back
    b += '\n'
  }

}//Markdown
