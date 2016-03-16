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
  protected def attributesStockRender(attrs: MarkAttributes) {
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
    b ++= "</"
    b ++= attrs.resolvedTagname
    b += '>'
  }

  // used for hr
  def renderBlockSelfClosingMark(attrs: MarkAttributes) {
    b += '<'
    b ++= attrs.resolvedTagname
    attributesStockRender(attrs)
    b ++= "/>"
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
  }

  def renderParagraphClose(name: String) {
    b ++= "</"
    b ++= name
    b += '>'
  }


  def renderTextParagraphOpen() {
    b ++= "<p>"
  }

  def renderTextParagraphClose() {
    b ++= "</p>"
  }


  // used for a, i, b, span
  def renderInlineOpen(attrs: MarkAttributes) = {
    // catch literal, ignore
    if (attrs.resolvedTagname != InlineLiteralTagname) {
      b += '<'
      b ++= attrs.resolvedTagname
      attributesStockRender(attrs)
      b += '>'
    }
  }


  def renderInlineClose(
    attrs: MarkAttributes
  )
  {
    val name  = attrs.resolvedTagname

    // catch literal, ignore
    if (name != InlineLiteralTagname) {
      b ++= "</"
      b ++= name
      b += '>'
    }
  }

  // Used for img
  def renderInlineSelfClosingMark(attrs: MarkAttributes) {
    b += '<'
    b ++= attrs.resolvedTagname
    if (attrs.klass != "") {
      b ++= " class=\""
      b ++= attrs.klass
      b ++= "\""
    }
    if (attrs.text != "") {
      b ++= " alt=\""
      b ++= attrs.text
      b ++= "\""
    }
    if (attrs.url != "") {
      b ++= " src=\""
      b ++= attrs.url
      b ++= "\""
    }
    b ++= "/>"
  }

}//HTML



object HTML
{
// val data = tml.FileReader("/home/rob/Code/scala/TML/text/GUIDE")
  // val r = tml.HTML.toWebpage("Guide", "/home/rob/Code/scala/TML/text/lib/doc.css",  p.result())
  /** Builds an html 5 web page from inner html.
    * 
    * Wraps the given contents in head/body/article tags and often-used
    * information.
    *
    * The page is HTML 5 and explicitly encoded as "UTF-8". It will
    * also display ASCII/Latin encodings.
    *
    * This method is intended for local testing or generation.  More
    * sophisticated needs will require more options than this method
    * provides.
    *
    * @param title
    * @param description
    * @param js
    * @param css
    * @param contents
    */
  def toWebpage(
    title: String,
    description: String,
    js: Traversable[String],
    css: Traversable[String],
    contents: String
  )
      : String =
  {
    val b = new StringBuilder()

    b ++= """<!DOCTYPE html><html><head>"""


    if (!title.isEmpty) {
      b ++= "<title>"
      b ++= title
      b ++= "</title>"
    }

    b ++= """<meta http-equiv="content-type" content="text/html; charset=UTF-8" />"""

    if (!description.isEmpty) {
      b ++= """<meta name="description" content=""""
      b ++= title
      b ++= """" />"""
    }

    if (!js.isEmpty) {
      js.foreach{ s =>
        b ++= """<script type="text/javascript" src=""""
        b ++= s
        b ++= """"></script>"""
      }
    }

    if (!css.isEmpty) {
      css.foreach{ s =>
        b ++= """<link rel="stylesheet" type="text/css" media="screen" href=""""
        b ++= s
        b ++= """"/>"""
      }
    }

    b ++= """</head><body><article>"""
    b ++= contents
    b ++= """</article></body></html>"""

    b.result()
  }

  /** Builds an html 5 web page from inner html.
    * 
    * Wraps the given contents in head/body tags and often-used
    * information.
    *
    * The page is HTML 5 and explicitly encoded as "UTF-8". It will
    * also display ASCII/Latin encodings.
    *
    * This method is intended for local testing or generation.  More
    * sophisticated needs will require more options than this method
    * provides.
    *
    * @param title
    * @param css
    * @param contents
    */
  def toWebpage(
    title: String,
    css: String,
    contents: String
  )
      : String =
  {
    toWebpage(
      title,
      "",
      Seq.empty,
      Seq(css),
      contents
    )
  }

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
