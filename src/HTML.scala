package tml



/** Parses TML markup to generate HTML.
  *
  * Direct-mapped HTML. Only contents are tagname
  * definitions/allocations, rendering, and systematic tagname
  * aliases.
  *
  * The only attributes available are,
  *
  * 'class' shortcut attribute renders as 'class="..."'
  *
  * For inline tagname 'a'
  * attribute1 = 'href="..."'
  *
  * For inline self-closing (tagname 'img')
  *  attribute1 'src="..."'
  * or...
  *  attribute1 = 'alt="..."', attribute2 'src="..."'
  *
  * NB: title attribute = "Relying on the title attribute is currently
  * discouraged" W3C HTML 5.1
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
    "italic" -> "i",
    "emphasis" -> "em"
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
  protected def classAttributeRender(md: MarkData)
  {
    if (md.klass != "") {
      ot ++= " class=\""
      ot ++= md.klass
      ot ++= "\""
    }

  }

  protected def renderAttribute(
    ot: OutputTarget,
    name: String,
    value: String
  )
  {
    ot += ' '
    ot ++= name
    ot ++= "=\""
    ot ++= value
    ot ++= "\""
  }


  // div, main, section, article, aside
  // ol, ul, dl,
  // li, dd, dt, blockquote
  def renderBlockOpen(md: MarkData)
  {
    ot += '<'
    ot ++= md.resolvedTagname
    classAttributeRender(md)
    ot += '>'

    logger.attributeRangeWarning(md, 0)
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
    classAttributeRender(md)
    ot ++= "/>"

    logger.attributeRangeWarning(md, 0)
  }

  // used for h?, pre
  def renderParagraphOpen(md: MarkData)
  {
    // fix the headline tag name
    // headlines are a little complex.
    // If there is no prefixed control char in the name,
    // then the tag name was given or defaulted, so do nothing.
    // If prefixes exist, they are counted to form the
    // resolvedTagname.
    if (md.control == '=') {
      val (ctrls, tag) = md.splitTagControls
      md.resolvedTagname =
        if (tag.size > 0) tag
        else "h" + (ctrls.size + 1)
    }

    ot += '<'
    ot ++= md.resolvedTagname
    classAttributeRender(md)
    ot += '>'

    logger.attributeRangeWarning(md, 0)
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
      classAttributeRender(md)

      if (md.resolvedTagname == "a") {
        if (md.params.size > 0) {
          renderAttribute(ot, "href", md.params(0))
        }
        logger.attributeRangeWarning(md, 1)
      }
      else logger.attributeRangeWarning(md, 0)

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
    classAttributeRender(md)

    if (md.params.size == 1) {
      renderAttribute(ot, "src", md.params(0))
    }
    else {
      if (md.params.size > 1) {
        renderAttribute(ot, "alt", md.params(0))
        renderAttribute(ot, "src", md.params(1))
      }
    }

    ot ++= "/>"

    logger.attributeRangeWarning(md, 1, 2)
  }

}//HTML



object HTML
    extends ParserCompanion[HTML]
{

  // val i = tml.InputIterator("/home/rob/Code/scala/TML/text/SPEC")
  // tml.FileReader("""/home/rob/Code/scala/TML/text/SPEC""")
  // tml.HTML(tml.FileReader.stream("""/home/rob/Code/scala/TML/text/SPEC"""))


  def builder(ot: OutputTarget)
      : HTML =
  {
    new HTML(ot)
  }

}//HTML
