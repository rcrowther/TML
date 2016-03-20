package tml



/** Parses TML markup to generate HTML.
*
* Only additions to TML are some tagname definitions, rendering,  and tagname aliases.
  */
class HTML(val ot: OutputTarget)
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
   ot ++= "</"
   ot ++= md.resolvedTagname
   ot += '>'
  }

  // used for hr
  def renderBlockSelfClosingMark(md: MarkData)
 {
   ot += '<'
   ot ++= md.resolvedTagname
    attributesStockRender(md)
   ot ++= "/>"
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
  }

  def renderParagraphClose(md: MarkData)
{
   ot ++= "</"
   ot ++= md.resolvedTagname
   ot += '>'
  }


  def renderTextParagraphOpen()
 {
   ot ++= "<p>"
  }

  def renderTextParagraphClose()
 {
   ot ++= "</p>"
  }


  // used for a, i, b, span
  def renderInlineOpen(md: MarkData)
{
    // catch literal, ignore
    if (md.resolvedTagname != InlineLiteralTagname) {
     ot += '<'
     ot ++= md.resolvedTagname
      attributesStockRender(md)
     ot += '>'
    }
  }


  def renderInlineClose(
    md: MarkData
  )
  {
    val name  = md.resolvedTagname

    // catch literal, ignore
    if (name != InlineLiteralTagname) {
     ot ++= "</"
     ot ++= name
     ot += '>'
    }
  }

  // Used for img
  def renderInlineSelfClosingMark(md: MarkData) 
{
   ot += '<'
   ot ++= md.resolvedTagname
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
   ot ++= "/>"
  }

}//HTML



object HTML
extends ParserCompanion[HTML]
{

  // val i = tml.InputIterator("/home/rob/Code/scala/TML/text/SPEC")
  // tml.FileReader("""/home/rob/Code/scala/TML/text/SPEC""")
  // tml.HTML(tml.FileReader.stream("""/home/rob/Code/scala/TML/text/SPEC"""))

/*
  def apply(it: InputIterator)
  {
    println("running apply")
    val p = new HTML()
    p(it)
    p.blockBalance(fix = false)
    println
    println(p.logger.toText())
    println("out:")
    println(s"'${p.result()}'")
    println(p)
  }
*/

  def builder(ot: OutputTarget)
      : HTML =
  {
    new HTML(ot)
  }

}//HTML
