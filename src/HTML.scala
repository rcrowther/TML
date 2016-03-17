package tml



/** Parses TML markup to generate HTML.
  */
class HTML
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

  /** Test on construction that all marks are different.
    */
  verifyControlDefinitions()

  // Renderers
  /** Renders non-tag attributes.
    *
    * Produces double quoted values for a common map of attributes.
    */
  protected def attributesStockRender(md: MarkData)
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
    b ++= "</"
    b ++= md.resolvedTagname
    b += '>'
  }

  // used for hr
  def renderBlockSelfClosingMark(md: MarkData)
 {
    b += '<'
    b ++= md.resolvedTagname
    attributesStockRender(md)
    b ++= "/>"
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
  }

  def renderParagraphClose(md: MarkData)
{
    b ++= "</"
    b ++= md.resolvedTagname
    b += '>'
  }


  def renderTextParagraphOpen()
 {
    b ++= "<p>"
  }

  def renderTextParagraphClose()
 {
    b ++= "</p>"
  }


  // used for a, i, b, span
  def renderInlineOpen(md: MarkData)
{
    // catch literal, ignore
    if (md.resolvedTagname != InlineLiteralTagname) {
      b += '<'
      b ++= md.resolvedTagname
      attributesStockRender(md)
      b += '>'
    }
  }


  def renderInlineClose(
    md: MarkData
  )
  {
    val name  = md.resolvedTagname

    // catch literal, ignore
    if (name != InlineLiteralTagname) {
      b ++= "</"
      b ++= name
      b += '>'
    }
  }

  // Used for img
  def renderInlineSelfClosingMark(md: MarkData) 
{
    b += '<'
    b ++= md.resolvedTagname
    if (md.klass != "") {
      b ++= " class=\""
      b ++= md.klass
      b ++= "\""
    }
    if (md.text != "") {
      b ++= " alt=\""
      b ++= md.text
      b ++= "\""
    }
    if (md.url != "") {
      b ++= " src=\""
      b ++= md.url
      b ++= "\""
    }
    b ++= "/>"
  }

}//HTML



object HTML
{

  // val i = tml.InputIterator("/home/rob/Code/scala/TML/text/SPEC")
  // tml.FileReader("""/home/rob/Code/scala/TML/text/SPEC""")
  // tml.HTML(tml.FileReader.stream("""/home/rob/Code/scala/TML/text/SPEC"""))

  def apply(it: InputIterator)
  {
    println("running apply")
    val p = new HTML()
    p(it)
    p.blockBalance(fix = false)
    println
    println(p.errorLog.toText())
    println("out:")
    println(s"'${p.result()}'")
    println(p)
  }

  def apply()
      : HTML =
  {
    new HTML()
  }

}//HTML
