package tml


import annotation._, elidable._

import java.io.InputStreamReader




/** A text markup parser.
  *
  * This class contains the mechanics of the parsing. It needs
  * extending to provide methods for rendering.
  */
//TODO: Text position is broken. Needs paragraph detection.
abstract class Parser
    extends Definitions
{


  //---------------------
  // Parser definitions
  //---------------------

  private val blockStack = collection.mutable.Stack[MarkData]()
  // TODO: Neeed data, or just attributes?
  private var paragraphMark: MarkData = MarkData.textParagraph()
  private val inlineStack = collection.mutable.Stack[MarkData]()

  /** Stringbuilder for output.
    */
  //protected val b = new StringBuilder()

  protected val ot: OutputTarget

  /** The error log for this parser.
    */
//new ActiveLogger()
// Public so it can be set/unset, read,
//etc.
  var logger : Logger = Logger.inactive()

  protected var it = tml.InputIterator.empty
  protected var currentChar = '\u0000'




  //---------------------
  // Mark definitions
  //---------------------

/** An arbitary string to open a parse.
*
* Probably best not used on markup which can stand alone e.g. webpages.
* howeveer, it wwould be useful for texts with closing stanzas e.g. XML, JSON.
*
*/
def open: String



/** An arbitary string to close a parse.
*
* Probably best not used on markup which can stand alone e.g. webpages.
* howeveer, it wwould be useful for texts with closing stanzas e.g. JSON.
*/
def close: String

  /** Marks for block-based markup.
    *
    * The char key is the control character, recognised after
    * multiple newlines.  The values are defaults for the tag, used
    * when a double control character is found.
    *
    * Block marks in TML are pairs. They start with the mark, then
    * close on a repetition of the mark. Open marks must have some
    * form of attributes, close marks must never have attributes.
    *
    * An empty (default tag name) open mark can be created by
    * repeating the mark.
    *
    * See also
    */
  def BlockBracketedMarks: Map[Char, String]

  /** Marks for block-based markup with titles.
    *
    * The difference between these and `BlockBracketedMarks` is
    * that these parse the first line and send it to the
    * renderer.
    *
    * Block marks in TML are pairs. They start with the mark, then
    * close on a repetition of the mark. They detect open and close by the
    * presence of a title i.e. not an empty following line. 
*
* They do not respond to duplicated marks (no defaults) or attributes. They do not parse post-mark text. They are rendered through `renderBlockOpen` and `renderBlockClose`, carrying a single attribute
of the title line.
    *
    * See also
    */
  def BlockBracketedTitledMarks: Seq[Char]


  /** Modify block bracket tag names automatically.
    *
    * Tag names found in this map are converted from the value to the
    * key.
    *
    * The map can be used to convert abbreviations to full names
    * (e.g. 'bq' -> 'blockquote') and expanded names to abbreviations
    * (e.g. 'italic' -> 'i')
    */
  def blockBracketedTagnameAliases: Map[String, String]


  /** Nominates a block control to have unparsed content.
    *
    * Literal content is passed from the input to the output with no
    * intervention. For example, the nominated tagname is
    * suitable for implementing HTML &lt;pre&gt;.
    */
  val BlockBracketedLiteralMark: Char

  /** Marks for paragraph-based markup.
    *
    * The char key is the control character, recognised after
    * multiple newlines.  The values are defaults for the tag, used
    * when a double control character is found.
    *
    * Paragraph marks in TML start with the mark, then self-close on
    * a paragraph end (double newline) (which includes a new block).
    *
    * See also
    */
  def BlockParagraphDefaultedMarks: Map[Char, String]

  /** Nominates paragraph marks as not defaulting.
    *
    * If a char in this sequence matches one in
    * `BlockParagraphMarks`, the mark will never be allocated the
    * default name.
    *
    * The main reason for preventing defaulting is so the renderer
    * can look at the control and tag name directly, usually to use
    * the methods in [[MarkData]] to count the number of
    * controls.
    *
    * For example, a non defaulting paragraph mark can implement the
    * HTML markup &lt;h?&gt;.
    */
  def BlockParagraphNoDefaultMarks: Seq[Char]

  /** Mark for self-closing markup.
    *
    * Self-closing markup renders only one tag, not a pair.
    * Therefore it has no text content.
    *
    * The self-closing paragraph mark work as paragraph substitutes,
    * and are not surrounded by auto-paragraphing.
    */
  val BlockSelfClosingMark: Char
  def BlockSelfClosingMarkDefault: String

  /** Marks for self-closing markup.
    *
    * Self-closing markup renders only one tag, not a pair.
    * Therefore it has no text content.
    *
    * Self-closing marks are treated as inline, so will be
    *  surrounded by auto-paragraphing.
    */
  // NB: val because it must be stable, but can be overridden.
  val InlineSelfClosingMark: Char = '*'
  def InlineSelfClosingMarkDefault: String

  /** Modify inline bracket tag names automatically.
    *
    * Tag names found in this map are converted from the value to the
    * key.
    *
    * The map can be used to convert abbreviations to full names
    * (e.g. 'bq' -> 'blockquote') and expanded names to
    * abbreviations (e.g. 'italic' -> 'i')
    */
  def inlineBracketedTagnameAliases: Map[String, String]

  /** Marks for inline markup.
    *
    * The char key is the control character, recognised after
    * multiple newlines.
    *
    * Inline marks in TML are pairs. They start on
    * `InlineBracketOpenMark`, then close on `InlineBracketCloseMark`.
    *
    * An empty tag name will be given the default.
    *
    * See also
    */
  val InlineBracketOpenMark: Char = '['
  val InlineBracketCloseMark: Char = ']'
  def InlineMarkDefault: String

  /** Nominates an inline tagname to be parsed literally.
    *
    * Literal content is passed from the input to the output with no
    * intervention.
    */
  val InlineLiteralTagname: String = "?"



  /** Combined term for all side significant paragraph marks.
    */
  private lazy val BlockParagraphMarks: Seq[Char] = {
    val b = Seq.newBuilder[Char]
    b ++= BlockParagraphDefaultedMarks.map(_._1)
    b ++= BlockParagraphNoDefaultMarks
    b.result()
  }

  /** Combined term for all side significant marks.
    */
  private lazy val SideSignificantMarks: Seq[Char] = {
    val b = Seq.newBuilder[Char]
    b += BlockSelfClosingMark
    b ++= BlockBracketedMarks.map(_._1)
    b ++= BlockBracketedTitledMarks
    b ++= BlockParagraphMarks
    b.result()
  }


  //------------------
  // Render callbacks
  //------------------

  def renderBlockOpen(md: MarkData)
  def renderBlockClose(md: MarkData)

  def renderParagraphOpen(md: MarkData)
  def renderParagraphClose(md: MarkData)

  def renderInlineOpen(md: MarkData)
  def renderInlineClose(md: MarkData)

  def renderBlockSelfClosingMark(md: MarkData)
  def renderInlineSelfClosingMark(md: MarkData)

  def renderTextParagraphOpen()
  def renderTextParagraphClose()




  
  
  
  //---------
  // Utility
  //---------


  /** Throws a conditional elidable error.
    *
    * Used for some basic error checking.
    */
  @elidable(FINEST) def errorIf(cond: Boolean, msg: String) {
    if (cond) {
      throw new Exception(msg)
    }
  }


  /** Test mark definitions are unique.
    *
    * Due to initialisation being varied, this must be called from
    * implemented parsers.  The test is not necessary, but is little
    * overload, as the tests may be elided on compilation.
    */
//TODO: extend for BlockBracketedTitledMark
  @elidable(FINEST) def verifyControlDefinitions()
  {
    val bbc = BlockBracketedMarks.map(_._1).toSeq
    val bpc = BlockParagraphMarks

    val ict = bbc.intersect(bpc)

    // blocks

    // test no controls shared between block controls and paragraphs.
    errorIf(
      (
        !ict.isEmpty
      ),
      "BlockBracketedMarks and BlockParagraphMarks share control char definitions shared chars: $ict"
    )

    // test BlockSelfClosingMark not used in other block controls.
    errorIf(
      (
        bbc.contains(BlockSelfClosingMark)
          || bpc.contains(BlockSelfClosingMark)
      ),
      "BlockBracketedMarks or BlockParagraphMarks contains BlockSelfClosingMark char: $InlineBracketOpenMark"
    )
    //BlockParagraphNoDefaultMarks

    // inlines

    // test InlineSelfClosingMark not used in other block controls.
    errorIf(
      (
        InlineBracketOpenMark == InlineSelfClosingMark
          || InlineBracketCloseMark == InlineSelfClosingMark
      ),
      "InlineBracketOpenMark or InlineBracketCloseMark equivalent to InlineSelfClosingMark char: $InlineSelfClosingMark"
    )

    // test InlineBracketOpenMark not equvalent to InlineBracketCloseMark.
    errorIf(
      (InlineBracketOpenMark == InlineBracketCloseMark),
      "InlineBracketOpenMark is equivalent to InlineBracketCloseMark char: $InlineBracketOpenMark"
    )
  }



  //-----------------
  // General parsing
  //-----------------

  /** Move forward by one char.
    *
    * Never do this if there is a possibility `currentPos` is near
    * the string end, as it reads the buffer and will error.
    *
    * However, the parser ensures whitespace on the end of input, so
    * this can be used whenever the current character is not
    * whitespace (then there is always another readable character).
    */
  @inline private def forward() {
    currentChar = it.next()
  }


  /** Skip space.
    *
    * Skips until `currentChar` is not a space.
    */
  @inline private def skipSpace() {
    while (currentChar == ' ') {
      forward()
    }
  }

  /** Skip whitespace.
    *
    * Skips until `currentChar` is not whitespace.
    */
  @inline private def skipWhitespace() {
    while (currentChar == ' ' || currentChar == LineFeed) {
      forward()
    }
  }
  

  /** String build from the current position to a specified limit.
    *
    * Also quits on newline.
    * 
    * @param limiter a function returning false when the gathering should stop.
    * @return a string containing all gathered characters.
    */
  private def getUntil(limiter: (Char) => Boolean)
      : String =
  {
    val b = new StringBuilder()

    while (currentChar != LineFeed && !limiter(currentChar)) {
      b += currentChar
      forward()
    }
    b.result()
  }

  /** Read a line.
    *
    * Reads until `currentChar` is a LineFeed.
    *
    * @return the line as a string.
    */
  private def readLine()
      : String =
  {
    val b = new StringBuilder()

    while (currentChar != LineFeed) {
      b += currentChar
      forward()
    }
    b.result()
  }



  //----------------
  // TML Syntax
  //----------------

  /** Parses control attributes.
    *
    * Reads from `currentChar`. Stops on free white space (including
    *  newline) (but continues over space in text and url attribute
    *  markup).
    *
    * @param controlChar the char which triggered the attribute reading.
    * @param until parse to this limit.
    * @return a class representing the data recovered.
    */
  /*
   private def parseAttributes(md: MarkData)
   {
   // Get tagname
   md.tagName = getUntil(
   (c: Char) => { c == '.' || c == '{' || c == '[' || Character.isWhitespace(c) }
   )

   while (!Character.isWhitespace(currentChar)) {
   currentChar match {
   case '.' => {
   forward()
   md.klass = getUntil((c: Char) => { c == '{' || c == '[' || Character.isWhitespace(c) })
   }

   case '[' => {
   forward()
   md.text = getUntil((c: Char) => { c == ']' })
   // if newline, give warning, else move off the closing bracket
   if(currentChar == LineFeed) {
   logger.textAttributeClosedByNewline(
   it
   )
   }
   else forward()
   }

   case '{' => {
   forward()
   md.url = getUntil((c: Char) => { c == '}' })

   // if newline, give warning, else move off the closing bracket
   if(currentChar == LineFeed) {
   logger.urlAttributeClosedByNewline(
   it
   )
   }
   else forward()

   }

   case _ => {
   logger.unknownAttributeMark(
   it,
   currentChar
   )

   forward()
   }
   }
   }
   
   // Now on whitespace
   }
   */

  private def parseAttributes(md: MarkData)
  {
    //val t = System.nanoTime()
    val attsB = Seq.newBuilder[String]

    // Get tagname
    md.tagName = getUntil(
      (c: Char) => { c == '.' || c == '{' || Character.isWhitespace(c) }
    )

    while (currentChar == '.') {
      forward()
      attsB += getUntil(
        (c: Char) => {  c == '.' || c == '{' ||  Character.isWhitespace(c) }
      )
    }

    md.klass = attsB.result()

    attsB.clear()

    while (currentChar == '{') {
      forward()
      attsB += getUntil(
        (c: Char) => { c == '}' || c == LineFeed }

      )
      if (currentChar != LineFeed) forward()
      else {
        logger.bracketAttributeClosedByNewline(
          it
        )
      }
    }

    md.params = attsB.result()
    //println(s"time: ${System.nanoTime() - t}")
  }



  /** Unstack and render a block close.
    *
    * @param charPos index of the control char.
    * @return true if the close was accepted and given markup, else false
    */
  private def handleBlockClose(controlMark: Char)
      : Boolean =
  {
    //val controlMark = currentChar

    if (blockStack.isEmpty) {
      // Too many closes. Ignore.
      logger.emptyBlockStack(
        it,
        controlMark
      )
     ot+= controlMark

      false
    }
    else {
      //println("  block close")

      val stackTextMark = blockStack.head
      if (stackTextMark.control != controlMark) {
        //error, don't know why, ignore
        logger.misMatchBlockStack(
          it,
          stackTextMark,
          controlMark
        )
       ot+= controlMark

        false
      }
      else {
        // Is ok, render and dispose of the markdata
        renderBlockClose(blockStack.pop())
        true
      }
    }
  }

  /** Resolve, stack, and render a block open mark.
    */
  private def handleBlockOpen(md: MarkData)
  {
    //println(s"  group open md:$md")
    md.tagNameResolve(BlockBracketedMarks)

    // resolve against the name map
    val n = md.resolvedTagname
    md.resolvedTagname = blockBracketedTagnameAliases.get(n).getOrElse(n)

    blockStack.push(
      md
    )

    renderBlockOpen(md)
  }

  /** Stack, and render an unresolved block open mark.
*
* Used for 
    */
  private def handleUnresolvedBlockOpen(md: MarkData)
  {

    blockStack.push(
      md
    )
//println(s"handleUnresolvedBlockOpen $blockStack")
    renderBlockOpen(md)
  }
  
  /** Unstack and render a paragraph close.
    */
  private def handleParagraphClose()
  {
    // Is ok, render and dispose? of the markdata
    if (paragraphMark.isTextParagraph) renderTextParagraphClose()
    else renderParagraphClose(paragraphMark)
  }

  /** Resolve, stack, and render a paragraph.
    */
  private def handleParagraphOpen(md: MarkData)
  {
    //println(s"  paragraph open md:$md")

    // defend the no-default marks
    if (!BlockParagraphNoDefaultMarks.contains(md.control)) {
      md.tagNameResolve(BlockParagraphDefaultedMarks)
    }
    paragraphMark = md

    renderParagraphOpen(md)
  }

  /** Resolve and render an inline self-closing mark.
    */
  private def handleBlockSelfClose(md: MarkData)
  {
    md.tagNameResolve(BlockSelfClosingMarkDefault)
    renderBlockSelfClosingMark(md)
  }

  /** Un-note and render an inline close.
    */
  private def handleInlineClose()
  {
    if (inlineStack.isEmpty) {
      // Too many closes. Ignore.
      // this could be a space.

      logger.emptyInlineStack(
        it
      )

     ot+= currentChar
    }
    else {
      // Is ok, render and dispose of the markdata
      renderInlineClose(inlineStack.head)
      inlineStack.pop()
    }
  }



  /** Resolve, note, and render a inline open.
    */
  private def handleInlineOpen(md: MarkData)
  {
    md.tagNameResolve(InlineMarkDefault)

    // resolve against the name map
    val n = md.resolvedTagname
    md.resolvedTagname = inlineBracketedTagnameAliases.get(n).getOrElse(n)

    inlineStack.push(md)

    renderInlineOpen(md)
  }

  /** Resolve and render an inline self-closing mark.
    */
  private def handleInlineSelfClose(md: MarkData)
  {
    md.tagNameResolve(InlineSelfClosingMarkDefault)
    renderInlineSelfClosingMark(md)
  }




  //---------
  // Parsers
  //---------

  private def parseBlockSelfClose()
  {
    //val controlPos = it.pos
    val md = MarkData(currentChar, it)

    // forward off the mark
    forward()

    parseAttributes(md)
    handleBlockSelfClose(md)
    skipSpace()
    parsePostMarkParagraph()
  }
  
  

  /** Parse a block open.
    *
    * The cursor must be on the control character. Parses any
    * following unmarked paragraph.
    *
    * @param firstChar the character from the index before `from`.
    * @param from index to start parsing. Should be on the name position,
    *  not the opening control character.
    * @return The index to restart
    * parsing. On error this will be `from`. The index after a parsed name may
    * match or exceed until.
    */
  private def parseBlockOpen()
  {
    // Stash for errors
    //val startPos = it.pos
    //val controlMark = currentChar
    //println(s"controlMark:$controlMark")
    val md = MarkData(currentChar, it)

    // Step on and read any data
    forward()

    parseAttributes(md)
    handleBlockOpen(md)
    skipSpace()

    if (md.control == BlockBracketedLiteralMark) {
      // parses literal block
      parseBlockLiteral()
    }
    else {
      // parse post control material
      parsePostMarkParagraph()
    }
  }
  
  /** Parses a block close.
    *
    * The cursor must be on the control character. Parses any
    * following unmarked paragraph.
    */
  private def parseBlockClose()
  {
    // if suceed, move off and skip space,
    // else treat control as start of unmarked paragraph
    if(handleBlockClose(currentChar)) {
      forward()
      
      // Now on whitspace
      skipSpace()
    }

    // parse post control material
    // if handing fails, processes the mark also
    parsePostMarkParagraph()
  }


  def parseBlockBracketedTitledMark(controlMark: Char) {
val controlChar = controlMark
//println(s"parseBlockBracketedTitledMark $controlMark")
    // Step on and read the line...
    forward()
val line = readLine().trim()
if (!line.isEmpty) {
// open
    val md = MarkData(controlChar, it)
md.params = Seq(line)
    handleUnresolvedBlockOpen(md)
// leave cursor on the linefeed
}
else {
// close
handleBlockClose(controlChar)
// leave cursor on the linefeed
//forward()
}
}

  /** Parse a block without parsing the contents.
    *
    * Parses the opening control, then seeks an end and prints the
    * close tag.
    *
    * The block contents are transferred direct from the input.  TML
    * codes within the block will not be triggered, escaping is
    * ignored, and whitespace will not be modified (according to TML
    * rules).
    */
  private def parseBlockLiteral()
  {
    //println(s"parseBlockLiteral currentChar: $currentChar")

    // now in space after attributes
    skipSpace()

    if (currentChar == LineFeed) forward()

    // ...parse the contents
    
    // setup prevChar
    var prevChar = currentChar
    var prevSignificantIsNewline = (currentChar == LineFeed)

    // forward off the first char
    // NB: can succed even near EOF, because this method should
    // not be entered except after parsing attributes,
    // which would stall on the newline previous to EOF
    forward()

    // NB: needs an EOF test, as skips newlines
    while(
      currentChar != EOF
        && !(
          prevSignificantIsNewline
            && currentChar == BlockBracketedLiteralMark
            && Character.isWhitespace(it.lookForward)
        )
    )
    {
      // print through
     ot+= prevChar
      prevChar = currentChar
      
      // update linecount
      // (not in main loop (automatic update) in literal)
      // if (prevChar == LineFeed) {
      //   lineCount += 1
      //   currentLinePos = it.pos
      // }

      // Don't update if current is a space, previous newlines
      // can still cause a block end
      /*
       if (prevChar != ' ') {
       prevSignificantIsNewline = (prevChar == LineFeed)
       }
       */
      prevSignificantIsNewline = (
        prevChar == LineFeed ||
          (prevChar == ' ' && prevSignificantIsNewline)
      )
      
      forward()
    }

    // Handles block close then forwards.
    // NB: Have already stepped over the paragraph-closing newline
    // so main loop catch will not work.
    // NB: Double EOF string tail protects agsinst EOF.
    if (currentChar == EOF) {
      // stack head must be there,...
      logger.overrunInlineLiteral(
        blockStack.head
      )
    }
    else {
      parseBlockClose()
    }

    // back off one char, and pretend is LineFeed.
    // if on EOF, puts the parser on the newline preceeding
    // EOF, where main loop code can handle.
    // if on a close, puts the parser on whatever preceeds. It may be
    // a space, but we know it was side significant. Main loop code
    // can handle the close (and also parse following paragraph,
    // etc.).

    //TODO: is popping?
    //currentPos = currentPos - 1
    //currentChar = LineFeed
  }


  /** Parse a paragraph.
    *
    * Paragraphs are a self closing on a newline.
    *
    * This method leaves the cursor in block-level whitespace
    * (which may or may not be a newline).
    */
  private def parseBlockParagraph(controlMark: Char)
  {
    //println(s"  parseBlockParagraph controlMark:$controlMark")
    // Stash for errors
    val md = MarkData(controlMark, it)

    // Step on and read any data
    forward()

    parseAttributes(md)
    handleParagraphOpen(md)
    skipSpace()
    // stops on a lineend.
    parseParagraphContent()
    
    handleParagraphClose()
  }



  /** Parses an inline close.
    *
    * The cursor must be on the control character.
    */
  private def parseInlineClose()
  {
    handleInlineClose()
    // Step off the control
    // Can parse from here, whatever e.g. paragraph ending
    // newline --- the space is not a terminal, the char is.
    forward()
  }

  /** Parse an inline open.
    *
    * The cursor must be on the initial control character.
    * 
    * The cursor will usually finish at the the end of space after the
    * attribute.  In the case of literals, it will finish in the end
    * of space after the closing delimiter.
    * 
    * The final char may be EOF or a newline.
    */
  private def parseInlineOpen()
  {
    //println(s"parseInlineOpen currentChar: $currentChar")

    val md = MarkData(InlineBracketOpenMark, it)
    forward()
    parseAttributes(md)
    handleInlineOpen(md)

    //println(s"parseInlineOpen md: $md")
    skipSpace()

    // Now in post-attribute space skip.
    // parseInlineLiteral walks the inline brackets, contents, stops on close mark.
    // ...or let inline loop continue
    if (md.tagName == InlineLiteralTagname) parseInlineLiteral()

  }


  /** Parse inline text without parsing the contents.
    *
    * Parses the opening control, then seeks an end and prints the
    * close tag.
    *
    * The block contents are transferred direct from the input.  TML
    * codes within the block will not be triggered, escaping is
    * ignored, and whitespace will not be modified (according to TML
    * rules).
    */
  // Not stacked?
  private def parseInlineLiteral()
  {
    // println(s"parseInlineLiteral currentChar: $currentChar")

    // NB: no need to handle EOF
    // searches for newline, and should never start on EOF
    while (currentChar != InlineBracketCloseMark
      && currentChar != LineFeed) {
      // output
     ot+= currentChar
      forward()
    }
    
    //println(s"parseInlineLiteral currentChar: $currentChar")

    // leave on control - loop catches on next round
    // ... last char\ dropped, so back to paragraph code, no need to do anything,
    // even spaces.

  }

  private def parseInlineSelfClose()
  {
    //val controlPos = it.pos
    val md = MarkData(InlineSelfClosingMark, it)
    forward()
    parseAttributes(md)

    // NB: Attributes ends on the delimiting space.
    handleInlineSelfClose(md)

    // at post attribute space end
    // if stopped because of newline, this will quit the
    // inlining, so don't cross.
    // otherwise, cross the space delimiter.
    if (currentChar != LineFeed) forward()
  }



  //------------
  // Paragraphs
  //------------

  /** Parses paragraph contents.
    * 
    * Assumes currentPos on content. Crosses any spacing, and handles
    * inline marks.
    * 
    * Stops on a linefeed.
    */
  // used directly for paragraph (substitutes)
  private def parseParagraphContent()
  {

    while (currentChar != LineFeed) {

      if (currentChar == ' ') {
        // Skip the space. If newlines present, quit the paragraphing,
        // else replace with a space.
        skipSpace()
        if (currentChar != LineFeed) ot+= ' '
      }
      else {

        // not whitespace
        // Catch inline marks
        currentChar match {
          case InlineBracketOpenMark => parseInlineOpen()
          case InlineBracketCloseMark => parseInlineClose()
          case InlineSelfClosingMark => parseInlineSelfClose()
          // is a free-text char
          // output
          case _ => {
           ot+= currentChar
            forward()
          }
        }
      }
    }

    // If inline stack not exhausted
    // then is closing on newlines
    for (i <- 0 until inlineStack.size) {
      val md = inlineStack.pop()

      // NB: this positioning is ok as a paragraph is one line.
      logger.inlineClosedByNewline(
        it,
        md
      )
      renderInlineClose(md)
    }

  }
  
  /** Parse a paragraph after an opening side-significant mark.
    * 
    * These paragraphs have inline parsing, but do not react to
    * side-significant marks. They may be paragraphed tagged.
    * 
    * Initial space is skipped. If it finds newline,
    * it will not do anything or move.
    *
    * Ends on newline. Thus returns to block level.
    */
  // For use after marks. Falls through to block handline -
  // ends on newline
  private def parsePostMarkParagraph()
  {
    // defend against generating unnecessary (empty) paragraph tags
    if (currentChar != LineFeed) parseUnmarkedParagraph()
  }



  
  /** Parse a paragraph with no opening side-significant mark.
    * 
    * Brackets the contents in paragraph marks.
    * 
    * Asssumes currentPos on content. Compresses spacing, and handles
    * inline marks.  Stops on a linefeed.
    * 
    * Used for unmarked paragraphs. See `parse`. Used for post-mark
    * paragraphs, see `parsePostMarkParagraph`. Also used for block
    * error recovery.
    */
  private def parseUnmarkedParagraph()
  {
    //println(s"parseUnmarkedParagraph currentChar: $currentChar")

    // it's always free text
    paragraphMark = MarkData.textParagraph()
    renderTextParagraphOpen()


    parseParagraphContent()

    // Process close marks
    // current char is the closing newline.
    // NB: Don't matter how block level space is handled,
    // so long as no newlines stepped over.
    handleParagraphClose()
  }


  private def parseSideSignificant()
  {
   // println(s"  parseSideSignificant currentChar:$currentChar")
    // in rough order of liklyhood?
    if (BlockBracketedMarks.contains(currentChar)) {
      //println(s"peek: ${it.lookForward}")
      // handles either mark and, for both, any following unsignificant paragraph
      // Short lookahead.
      // Need at least one extra non-whitespace char for an open
      // ...any whitespace is a close
      if (Character.isWhitespace(it.lookForward)) {
        parseBlockClose()
      }
      else {
        parseBlockOpen()
      }
    }
    else {
      if (BlockParagraphMarks.contains(currentChar)) {
        // handles the open mark, and following paragraph
        parseBlockParagraph(currentChar)
      }
else {
if (BlockBracketedTitledMarks.contains(currentChar)) {
            parseBlockBracketedTitledMark(currentChar)
}
      else {
        currentChar match {
          // literal block
          case BlockBracketedLiteralMark => {
            // parses open mark, contents, close mark,
            // following unsignificant paragraph
            parseBlockLiteral()
          }
          case BlockSelfClosingMark => {
            // process self close
            // handles the mark, and following unsignificant paragraph
            parseBlockSelfClose()
          }
          case _ => {
            // should be unreachable
          }
        }
}
      }
    }
  }
  
  /** Processes a string.
    *
    * Expects the line to contain LF  + EOF on the end.
    */
  def apply(itIn: InputIterator)
  {
// add immediately
// not on construction, class initialises too early.
ot ++= open

    // Ensure the parser ends in the following seq of 2 chars.
    // NB: This parser makes no attempt to test the size of it's input,
    //  it defines limits by setting control characters to the
    // end of input. In Java this can be expensive, as it is an
    // immutable string copy.
    // - LF
    // because most TML commands finish with LF
    // so this ensures trailing controls are completed.
    // - EOF
    // For the main loop detection.
    // ...this allows us to bump limit when processing forward.
    // Test for LF, or fail


    // this is ok to read '0', the string should always have EOF appended.
    it = itIn
    currentChar = it.next()


    while (currentChar != EOF) {
//println(s"currentChar: ${currentChar.toInt}$currentChar")
//println ( (currentChar == EOF) )
//println(s"SideSignificantMarks $SideSignificantMarks")
      // Jump whitespace for something useful
      // must allow to go to loop test again here, see if
      // limit reached, or spurious paragraph markup
      // will be printed on end of output.
      // NB: No spaces necessary or printed at block level
      //println(s"block level whitespace now: $currentChar")
      skipSpace()
      

      if (currentChar == LineFeed) {
        //lineCount += 1
        //currentLinePos = currentPos
        forward()
      }
      else {
        
        // Significant content
        // methods following should end on block level.

        // handle block marks, else treat as paragraph.
        //println(s"skipped whitespace lastMarkWasNewline:$lastMarkWasNewline now: $currentChar")
        //println(s"main block loop: '$currentChar'")


        if (SideSignificantMarks.contains(currentChar)) {
          parseSideSignificant()
        }
        else {
          // unmarked paragraph
          parseUnmarkedParagraph()
        }

      }
    }
//println("EOP")
ot ++= close
  }




  
  /** Attempts fixes on bracketing.
    *
    * Exhausts the mark stores, writing tags as necessary. Since TML
    * will always place marks it has detected onto stacks, this will
    * produce balanced markup. However, a final imbalance in the stack
    * implies some problem in markup or detection, and this method can
    * not protect against unintended or missed markup. But the result
    * will always have balanced tags.
    */
  def blockBalance(fix: Boolean)
  {
    // Inline is automatic, should always close

    for (i <- 0 until blockStack.size) {
      val md = blockStack.head
      logger.unclosedBlock(
        md
      )

      // Is ok, render and dispose of the markdata
      if (fix) renderBlockClose(md)
      blockStack.pop()

    }
  }

  /** Resets the parser to a starting condition.
    *
    * Clears the builder, stacks, errorlog, etc.
    */
  def clear()
  {
    currentChar = '\u0000'
    blockStack.clear()
    paragraphMark = MarkData.textParagraph()
    inlineStack.clear()
    ot.clear()
    logger.clear()
  }

  /** Returns a string representating this parser.
    *
    * The string shows details of stack and other parsing state.
    */
  override def toString()
      : String =
  {
    val b = new StringBuilder()
    b ++= "Parser("
    b ++= "blockStack:"
    b append blockStack
    b ++= ", paragraphMark"
    b append paragraphMark
    b ++= ", inlineStack:"
    b append inlineStack
    b += ')'
    b.result
  }

  //def result() : String = b.result()

}//Parser



