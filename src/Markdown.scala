package tml



/** Translation of TML to Markdown.
  *
  * One way to do this is to convert to HTML then pass in. Markdown
  * should pass HTML untouched.
  *
  * This class will attempt to make `Markdown` marks from
  * TML. `Markdown` is more idiomatic than HTML, and easier
  * to edit. `Markdown` is more expressive than TML, so the results
  * will be limited.
  *
  * *Warnings*
  *  - Don't use literals for block-level codeblocks (the Parser will
  *  fail to indent correctly).
  *  - Images require two attributes `alt text` then `link/anchor`
  *  - TML handles unlimited nesting.
  * 
  * The parser will not, 
  *  - handle the attribute 'title' in links, or reference links.
  *  - escape codeblock contents, an automatic Markdown effect. Use a
  *  preprocessing stage,
  *
  * {{{
  * tml.Utils.filterWhen('?', "codeblock", tml.HTMLUtils.toDisplay _, <data>)
  * }}}
  *
  * to mimic this behaviour.

  *
  * The parser will, 
  *  - parse TML list items at both block and paragraph level.
  *  - parse ordered lists to ordered list Markdown.
  *  - handle nesting (mostly) of blockquotes, codeblocks, lists,
  *  and anonymous paragraphs.
  *
  * The parser cannot,
  *  - parse the `Markdown` `&lt;br>` spacing trick
  *  - defend consecutive blocklevel items such as lists and
  *  blockquotes, which in `Markdown` blend together. 
  * 
  * It is best to restrain source markup to common TML idoms. Markdown has
  * little of the flexibility of TML, and TML guesses the intention
  * from tagnames (except for headlines, where the detection is by the
  * control mark `=`). Override tagnames, and the result may be
  * caotic.
  *
  * If a mark is unrecognised (many marks can be valid TML, but not
  * `Markdown`) the parser reports an
  * `unrecognisedMarkWarning`.
  *
  * [[https://daringfireball.net/projects/markdown]]
  */
// TODO: Escapes, liss, blockquote, headline
//TODO: Perhaps should override HTML
class Markdown(val ot: OutputTarget)
    extends Parser
    with HTMLDefinitions
{
  // Gather the indent as an copyable object,
  // helpful for Markdown blockquoting
  private var indent: String = ""

  @inline
  private def indentInc() = { indent = indent + "    " }

  @inline
  private def indentBlockQuoteInc() = { indent = indent + ">   " }

  @inline
  private def indentDec() = {
    if (!indent.isEmpty) {
      indent = indent.substring(0, indent.size - 4)
    }
  }


  // Block-level list items items must be indent-decremented,
  // but other items can be at indent
  @inline
  private def decrementedIndent()
      : String =
  {
    if (!indent.isEmpty) {
      indent.substring(0, indent.size - 4)
    }
    else indent
  }



  // Markdown's lists use a paragraphing syntax.
  // To enable from TML bracket marks, need to carry a little state.

  // state of in block-level list item
  private var blockListitemFirst = false
  private var orderedListCount = collection.mutable.MutableList.empty[Int]
  private var inOrderedList = false

  // Marksdown likes double newline spacing
  // --- this is here to tighten TML literals.
  private var singleNewlineParagraphing = false

  private def unrecognisedMarkWarning(
    md: MarkData
  )
  {
    logger.rendererWarning(
      md,
      s"Mark unrecognised by Markdown parser mark: ${md.toMarkString()}",
      "ignored"
    )
  }



  // div, main, section, article, aside
  // ol, ul, dl,
  // li, dd, dt, blockquote
  def renderBlockOpen(md: MarkData)
  {
    // If first item in a list is not an anonymous paragraph,
    // need to cancel
    blockListitemFirst = false
    // No longer...
    inOrderedList = false

    // all write at least one newline to separate
    // from previous material  (TML streams output)
    md.resolvedTagname match {
      // Markdown blockquote
      case  "blockquote" => {
        ot ++= "\n\n"
        indentBlockQuoteInc()
        //NB: paragraph handles paragraph placement
      }
      case  "ol" => {
        ot += '\n'
        inOrderedList = true
        orderedListCount += 1
        indentInc
      }
      case  "ul" => {ot += '\n'; indentInc}
      case  "dl" => {ot += '\n'; indentInc}
      case  "li" => {
        ot += '\n'
        blockListitemFirst = true
      }
      case "codeblock" => {
        ot ++= "\n\n"
        indentInc
        singleNewlineParagraphing = true
      }
      case _ => unrecognisedMarkWarning(md)
    }

  }

  def renderBlockClose(
    md: MarkData
  )
  {
    md.resolvedTagname match {
      // Markdown blockquote
      case  "blockquote" => indentDec
      case  "ol" => {
        indentDec
        orderedListCount.dropRight(1)
        inOrderedList = false
      }
      case  "ul" => indentDec
      case  "dl" => indentDec
      case  "li" =>
      case "codeblock" => {
        indentDec
        singleNewlineParagraphing = false
      }
      case _ => unrecognisedMarkWarning(md)
    }
  }

  // used for h?, pre
  def renderParagraphOpen(md: MarkData)
  {
    // If first item in a list is not an anonymous paragraph,
    // need to cancel
    blockListitemFirst = false

    // fix the headline tag name
    if(md.control == '=') {
      // extra inline for clarity
      ot ++= "\n\n\n"

      // headline
      ot ++= indent
      ot ++= ("#" * (md.tagName.size + 1))
      ot += ' '
    }
    else {
      if (md.resolvedTagname == "li") {
        ot += '\n'
        ot ++= decrementedIndent()

        if (inOrderedList) {
          val nStr = orderedListCount.head.toString
          ot ++= nStr
          //TODO: Adaptfor double figures
          ot += '.'
          ot ++= (" " * (3 - nStr.size))
          // update the list number
          orderedListCount.update(
            orderedListCount.size - 1,
            orderedListCount.head + 1
          )
        }
        else ot ++= "-   "
      }
      else unrecognisedMarkWarning(md)
    }


  }

  def renderParagraphClose(md: MarkData)
  {
    // markdown headlines need closing...
    // (well, are best closed in some way)
    if(md.control == '=') {
      // headline
      ot ++= " #"
    }
  }

  // used for hr
  def renderBlockSelfClosingMark(md: MarkData)
  {
    // Markdown hr
    if (md.resolvedTagname == "hr") {
      // space with extra newline
      ot ++= "\n\n* * *"
    }
    else unrecognisedMarkWarning(md)
  }

  // used for a, i, b, span
  def renderInlineOpen(md: MarkData)
  {

    // Markdown link
    // link = [an example](http://example.com/ "Title")
    md.resolvedTagname match {
      case "a" => {
        ot += '['
      }
      // should be ? control too
      case "codeblock" => ot ++= "``"
      case "em" => {
        ot += '*'
      }
      case _ => unrecognisedMarkWarning(md)
    }
  }


  def renderInlineClose(
    md: MarkData
  )
  {

    md.resolvedTagname match {
      case "a" => {
        ot ++= "]("
        if (md.params.isDefinedAt(0) && md.params(0) != "") {
          ot ++= md.params(0)
        }
        ot += ')'
      }
      case "codeblock" => ot ++= "``"
      case "em" => {
        ot += '*'
      }
      case _ => unrecognisedMarkWarning(md)
    }
  }

  // Used for img
  def renderInlineSelfClosingMark(md: MarkData)
  {

    // Markdown img
    //img = ![Alt text](/path/to/img.jpg "Optional title")
    if (md.resolvedTagname == "img") {

      if (md.params.size == 2) {
        ot += '!'

        ot += '['
        ot ++= md.params(0)
        ot += ']'

        ot += '('
        ot ++= md.params(1)
        ot += ')'

      }
      else logger.attributeRangeWarning(md, 2)
    }
    else unrecognisedMarkWarning(md)
    //println(s"render... '${b.result}'")
  }


  def renderTextParagraphOpen()
  {
    if (!blockListitemFirst) {
      // put the newline back, usually double for clarity
      if (!singleNewlineParagraphing) {
        ot ++= "\n\n"
      }
      else ot ++= "\n"

      ot ++= indent
    }
    else {
      // match TML's block level controls to Markdown's paragraphing
      // syntax by avoiding newlines for blockListitemFirst.
      // reduced indent to allow roon for preceeding mark
      ot ++= decrementedIndent()
      ot ++= "+   "
      blockListitemFirst = false
    }
  }

  def renderTextParagraphClose()
  {
    // do nothing
  }

}//Markdown



object Markdown
    extends ParserCompanion[Markdown]
{

  // val i = tml.InputIterator("/home/rob/Code/scala/TML/text/SPEC")
  // tml.FileReader("""/home/rob/Code/scala/TML/text/SPEC""")
  // tml.HTML(tml.FileReader.stream("""/home/rob/Code/scala/TML/text/SPEC"""))


  def builder(ot: OutputTarget)
      : Markdown =
  {
    new Markdown(ot)
  }

}//Markdown
