package tml


import annotation._, elidable._



/** A text markup parser.
  *
  */
//TODO: Text position is broken. Needs paragraph detection.
abstract class Parser()
{



  //---------------------
  // General definitions
  //---------------------

  // ASCII control code - ETX (end of text)
  val EOF: Char = 3
  val LineFeed: Char = 10
  val CarriageReturn: Char = 13

  // What is this? Is a backslash.
  //val EscapeChar: Char = 92
  // Newline11
  // Unix = LF
  // Win/ASCII = CR ~ LF
  //val CarrageReturn = 13



  //---------------------
  // Parser definitions
  //---------------------

  private val blockStack = collection.mutable.Stack[MarkData]()
  // TODO: Neeed data, or just attributes?
  private var paragraphMark: MarkData = MarkData.textParagraph()
  private val inlineStack = collection.mutable.Stack[MarkData]()

  /** Stringbuilder for output.
    */
  protected val b = new StringBuilder()

  private var errLog = new ErrorLog()

  private var in: String = ""
  private var currentChar = '\0'
  private var currentPos = 0

  /** Counts lines parsed.
    *
    * For error displays.
    */
  private var lineCount = 0

  /** Position of the start of the current line.
    *
    * When a string is indexed to seek for lines,
    * holds the index of the line currently displayed.
    *
    * For error displays (line position calculation).
    */
  private var currentLinePos = 0



  //---------------------
  // Mark definitions
  //---------------------

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
  def BlockParagraphMarks: Map[Char, String]

  /** Nominates paragraph marks as not defaulting.
    *
    * If a char in this sequence matches one in
    * `BlockParagraphMarks`, the mark will never be allocated the
    * default name.
    *
    * The main reason for preventing defaulting is so the renderer
    * can look at the control and tag name directly, usually to use
    * the methods in [[MarkAttributes]] to count the number of
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
    * `InlineBracketStartMark`, then close on `InlineBracketEndMark`.
    *
    * An empty tag name will be given the default.
    *
    * See also
    */
  val InlineBracketStartMark: Char = '['
  val InlineBracketEndMark: Char = ']'
  def InlineMarkDefault: String

  /** Nominates an inline tagname to be parsed literally.
    *
    * Literal content is passed from the input to the output with no
    * intervention.
    */
  val InlineLiteralTagname: String = "?"




  /** Combined term for all side significant marks.
    */
  private lazy val SideSignificantMarks: Seq[Char] = {
    val b = Seq.newBuilder[Char]
    b += BlockSelfClosingMark
    b ++= BlockBracketedMarks.map(_._1)
    b ++= BlockParagraphMarks.map(_._1)
    b.result()
  }

  /** Test on construbtion that all marks are different.
    */
  //ensureControlDefinitions()



  //------------------
  // Render callbacks
  //------------------

  def renderBlockOpen(attrs: MarkAttributes)
  def renderBlockClose(attrs: MarkAttributes)

  def renderParagraphOpen(attrs: MarkAttributes)
  def renderParagraphClose(name: String)

  def renderInlineOpen(attrs: MarkAttributes)
  def renderInlineClose(attrs: MarkAttributes)

  def renderBlockSelfClosingMark(attrs: MarkAttributes)
  def renderInlineSelfClosingMark(attrs: MarkAttributes)

  def renderTextParagraphOpen()
  def renderTextParagraphClose()




  
  
  
  //---------
  // Utility
  //---------

  def resetCursor()
  {
    currentPos = 0
    currentChar = in(currentPos)
  }

  /** Throws a conditional elidable error.
    *
    * Used for some basic error checking.
    */
  @elidable(FINEST) def errorIf(cond: Boolean, msg: String) {
    if (cond) {
      throw new Exception(msg)
    }
  }

  /** Returns the error log from this parser.
    */
  def errorLog: ErrorLog = errLog

  /** Represents a control char as a string.
    *
    * Converts whitespace into something human-readable.
    */
  private def controlMarkToString(controlMark: Char): String = {
    if (controlMark == ' ') {
      "<space>"
    }
    else {
      if (controlMark == LineFeed) {
        "<linefeed (13)>"
      }
      else {
        controlMark.toString
      }
    }
  }



  /** Test mark definitions are unique.
    */
  def ensureControlDefinitions()
  {
    val bbc = BlockBracketedMarks.map(_._1).toSeq
    val bpc = BlockParagraphMarks.map(_._1).toSeq

    val ict = bbc.intersect(bpc)
    errorIf(
      (
        !ict.isEmpty
      ),
      "BlockBracketedMarks and BlockParagraphMarks share control char definitions shared chars: $ict"
    )

    errorIf(
      (
        bbc.contains(BlockSelfClosingMark)
          || bpc.contains(BlockSelfClosingMark)
      ),
      "BlockBracketedMarks or BlockParagraphMarks contains BlockSelfClosingMark char: $InlineBracketStartMark"
    )

    errorIf(
      (InlineBracketStartMark == InlineBracketEndMark),
      "InlineBracketStartMark is equal to InlineBracketEndMark char: $InlineBracketStartMark"
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
  @inline def forward() {
    currentPos += 1
    currentChar = in(currentPos)
  }


  /** Skip space.
    *
    * Skips until `currentChar` is not a space.
    */
  @inline def skipSpace() {
    while (currentChar == ' ') {
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
  def getUntil(limiter: (Char) => Boolean)
      : String =
  {
    val b = new StringBuilder()

    while (currentChar != LineFeed && !limiter(currentChar)) {
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
  def parseAttributes(controlChar: Char)
      : MarkAttributes =
  {
    // Get name
    var ma = MarkAttributes()
    ma.control = controlChar
    ma.tagName = getUntil(
      (c: Char) => { c == '#' || c == '.' || c == '{' || c == '[' || Character.isWhitespace(c) }
    )

    while (!Character.isWhitespace(currentChar)) {
      currentChar match {
        case '#' => {
          forward()
          ma.id = getUntil((c: Char) => { c == '.' || c == '{' || c == '[' || Character.isWhitespace(c) })
        }
        case '.' => {
          forward()
          ma.klass = getUntil((c: Char) => { c == '#' || c == '{' || c == '[' || Character.isWhitespace(c) })
        }
        case '{' => {
          forward()
          ma.url = getUntil((c: Char) => { c == '}' })

          // if newline, give warning, else move off the closing bracket
          if(currentChar == LineFeed) {
            errLog.warning(
              lineCount,
              currentLinePos,
              currentPos,
              s"URL attribute closed by newline: unintended?"
            )
          }
          else forward()

        }
        case '[' => {
          forward()
          ma.text = getUntil((c: Char) => { c == ']' })
          // if newline, give warning, else move off the closing bracket
          if(currentChar == LineFeed) {
            errLog.warning(
              lineCount,
              currentLinePos,
              currentPos,
              s"Text attribute closed by newline: unintended?"
            )
          }
          else forward()

        }
        case _ => {
          val cm = controlMarkToString(currentChar)

          errLog += (
            lineCount,
            currentLinePos,
            currentPos,
            s"Unable to match an attribute mark to current char. Char found: '$cm'"
          )

          forward()
        }
      }
    }
    
    // Now on whitespace


    ma
  }




  /** Unstack and render a block close.
    *
    * @param charPos index of the control char.
    * @return true if the close was accepted and given markup, else false
    */
  def handleBlockClose(charPos: Int, controlMark: Char)
      : Boolean =
  {
    if (blockStack.isEmpty) {
      val cm = controlMarkToString(controlMark)
      // Too many closes. Ignore.
      errLog += (
        lineCount,
        currentLinePos,
        currentPos,
        s"Block stack is empty but a control mark is trying to close a block controlMark: '$cm'\nMark is ignored."
      )
      b += controlMark

      false
    }
    else {
      //println("  block close")

      val stackTextMark = blockStack.head.attrs.control
      if (stackTextMark != controlMark) {
        val cm = controlMarkToString(controlMark)
        val stm = controlMarkToString(stackTextMark)

        //error, don't know why, ignore
        errLog += (
          lineCount,
          currentLinePos,
          currentPos,
          s"Control mark '$cm' does not match mark on stack '$stm'.\nMark is ignored."
        )
        b += controlMark

        false
      }
      else {
        // Is ok, render and dispose of the markdata
        renderBlockClose(blockStack.head.attrs)
        blockStack.pop()
        true
      }
    }
  }

  /** Resolve, stack, and render a block open mark.
    */
  def handleBlockOpen(startPos: Int, attrs: MarkAttributes)
  {
    //println(s"  group open attrs:$attrs")
    attrs.tagNameResolve(BlockBracketedMarks)

    // resolve against the name map
    val n = attrs.resolvedTagname
    attrs.resolvedTagname = blockBracketedTagnameAliases.get(n).getOrElse(n)

    blockStack.push(
      MarkData(
        currentPos,
        attrs
      )
    )

    renderBlockOpen(attrs)
  }
  
  /** Unstack and render a paragraph close.
    */
  def handleParagraphClose()
  {
    // Is ok, render and dispose? of the markdata
    if (paragraphMark.isTextParagraph) renderTextParagraphClose()
    else renderParagraphClose(paragraphMark.attrs.resolvedTagname)
  }

  /** Resolve, stack, and render a paragraph.
    */
  def handleParagraphOpen(startPos: Int, attrs: MarkAttributes)
  {
    //println(s"  paragraph open cmark:$controlMark attrs:$attrs")

    // first, defend the noDefault mark
    if (!BlockParagraphNoDefaultMarks.contains(attrs.control)) {
      attrs.tagNameResolve(BlockParagraphMarks)
    }
    paragraphMark =
      MarkData(
        currentPos,
        attrs
      )

    renderParagraphOpen(attrs)
  }

  /** Resolve and render an inline self-closing mark.
    */
  def handleBlockSelfClose(controlPos: Int, attrs: MarkAttributes)
  {
    attrs.tagNameResolve(BlockSelfClosingMarkDefault)
    renderBlockSelfClosingMark(attrs)
  }

  /** Un-note and render an inline close.
    */
  def handleInlineClose()
  {
    if (inlineStack.isEmpty) {
      // Too many closes. Ignore.
      val cm = controlMarkToString(currentChar)

      errLog += (
        lineCount,
        currentLinePos,
        currentPos,
        s"Inline close mark but stack empty: mark: '$cm'\nMark is ignored."
      )

      b += currentChar
    }
    else {
      // Is ok, render and dispose of the markdata
      renderInlineClose(inlineStack.head.attrs)
      inlineStack.pop()
    }
  }



  /** Resolve, note, and render a inline open.
    */
  def handleInlineOpen(controlPos: Int, attrs: MarkAttributes)
  {
    attrs.tagNameResolve(InlineMarkDefault)

    // resolve against the name map
    val n = attrs.resolvedTagname
    attrs.resolvedTagname = inlineBracketedTagnameAliases.get(n).getOrElse(n)

    inlineStack.push(
      MarkData.inline(
        controlPos,
        attrs
      )
    )

    renderInlineOpen(attrs)
  }

  /** Resolve and render an inline self-closing mark.
    */
  def handleInlineSelfClose(controlPos: Int, attrs: MarkAttributes)
  {
    attrs.tagNameResolve(InlineSelfClosingMarkDefault)
    renderInlineSelfClosingMark(attrs)
  }




  //---------
  // Parsers
  //---------

  def parseBlockSelfClose()
  {
    val controlPos = currentPos

    // forward off the mark
    forward()

    val attrs = parseAttributes(InlineBracketStartMark)
    handleBlockSelfClose(controlPos, attrs)
    skipSpace()
    parsePostMarkParagraph()
  }
  
  

  /** Parse a block open.
    *
    * The cursor must be on the control character. Parses any following unmarked paragraph.
    *
    * @param firstChar the character from the index before `from`.
    * @param from index to start parsing. Should be on the name position,
    *  not the opening control character.
    * @return The index to restart
    * parsing. On error this will be `from`. The index after a parsed name may
    * match or exceed until.
    */
  def parseBlockOpen()
  {
    // Stash for errors
    val startPos = currentPos
    val controlMark = currentChar
    //println(s"controlMark:$controlMark")

    // Step on and read any data
    forward()

    val attrs = parseAttributes(controlMark)
    handleBlockOpen(startPos, attrs)
    skipSpace()

    if (attrs.control == BlockBracketedLiteralMark) {
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
    * The cursor must be on the control character. Parses any following unmarked paragraph.
    */
  def parseBlockClose()
  {
    // if suceed, move off and skip space,
    // else treat control as start of unmarked paragraph
    if(handleBlockClose(currentPos, currentChar)) {
      forward()
      
      // Now on whitspace
      skipSpace()
    }

    // parse post control material
    // if handing fails, processes the mark also
    parsePostMarkParagraph()
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
  def parseBlockLiteral()
  {
    println(s"parseBlockLiteral currentChar: $currentChar")



    // now in skipped space after attributes
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
            && Character.isWhitespace(in(currentPos + 1))
        )
    )
    {
      // print through
      b += prevChar
      prevChar = currentChar
      
      // update linecount
      // (not in main loop (automatic update) in literal)
      if (prevChar == LineFeed) {
        lineCount += 1
        currentLinePos = currentPos
      }

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
      errLog += (
        lineCount,
        currentLinePos,
        currentPos,
        s"Literal Block reached EOF without close\nStopping literal (will produce tail errors)."
      )


    }
    // back off one char, and pretend is LineFeed.
    // if on EOF, puts the parser on the newline preceeding
    // EOF, where main loop code can handle.
    // if on a close, puts the parser on whatever preceeds. It may be a space,
    // but we know it was side significant. Main loop code can handle the close (and also parse following paragraph, etc.).
    currentPos = currentPos - 1
    currentChar = LineFeed
  }


  /** Parse a paragraph.
    *
    * Paragraphs are a self closing on a newline.
    *
    * This method leaves the cursor in block-level whitespace
    * (which may or may not be a newline).
    */
  def parseBlockParagraph(controlMark: Char)
  {
    // Stash for errors
    val startPos = currentPos

    // Step on and read any data
    forward()

    val attrs = parseAttributes(controlMark)
    handleParagraphOpen(startPos, attrs)
    skipSpace()
    // stops on a lineend.
    parseParagraphContent()
    
    handleParagraphClose()
  }



  /** Parses an inline close.
    *
    * The cursor must be on the control character.
    */
  def parseInlineClose()
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
    * The cursor will usually finish at the the end of space after the attribute.
    * In the case of literals, it will finish in the end of space after
    * the closing delimiter. 
    * 
    * The final char may be EOF or a newline.
    */
  def parseInlineOpen()
  {
    println(s"parseInlineOpen currentChar: $currentChar")

    val controlPos = currentPos
    forward()
    val attrs = parseAttributes(InlineBracketStartMark)
    handleInlineOpen(controlPos, attrs)

    //println(s"parseInlineOpen attrs: $attrs")
    skipSpace()

    // Now in post-attribute space skip.
    // parseInlineLiteral walks the inline brackets, contents, stops on close mark.
    // ...or let inline loop continue
    if (attrs.tagName == InlineLiteralTagname) parseInlineLiteral()

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
  def parseInlineLiteral()
  {
    // println(s"parseInlineLiteral currentChar: $currentChar")

    // NB: no need to handle EOF
    // searches for newline, and should never start on EOF
    while (currentChar != InlineBracketEndMark
      && currentChar != LineFeed) {
      // output
      b += currentChar
      forward()
    }
    
    println(s"parseInlineLiteral currentChar: $currentChar")
    if (currentChar == LineFeed) {

      errLog.warning(
        lineCount,
        currentLinePos,
        currentPos,
        s"Inline literal closed by newline: unintended?"
      )

      //position on linefeed is fine for paragraph ending.
    }
    else {
      // leave on control - loop catches on next round
      // off the control
      //forward()
      // skip spaces
      //skipSpace()
      // back to paragraph code, no need to do anything,
      // even spaces.
    }
  }

  def parseInlineSelfClose()
  {
    val controlPos = currentPos
    forward()
    val attrs = parseAttributes(InlineBracketStartMark)

    // NB: Attributes ends on the delimiting space.
    handleInlineSelfClose(controlPos, attrs)

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
        if (currentChar != LineFeed) b += ' '
      }
      else {

        // not whitespace
        // Catch inline marks
        currentChar match {
          case InlineBracketStartMark => parseInlineOpen()
          case InlineBracketEndMark => parseInlineClose()
          case InlineSelfClosingMark => parseInlineSelfClose()
          // is a free-text char
          // output
          case _ => {
            b += currentChar
            forward()
          }
        }
      }
    }

    // If inline stack not exhausted
    // then is closing on newlines
    for (i <- 0 until inlineStack.size) {
      val d = inlineStack.pop()

// NB: this positioning is ok as a paragraph is one line.
      errLog.warning(
        lineCount,
        currentLinePos,
        d.startPos,
        s"Inline mark closed by newline: '${d.attrs.control}${d.attrs.tagName}'\nUnintended?"
      )
      renderInlineClose(d.attrs)
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
  def parsePostMarkParagraph()
  {
    // defend against generating unnecessary (empty) paragraph tags
    if (currentChar != LineFeed) parseUnmarkedParagraph()
  }



  
  /** Parse a paragrah with no opening side-significant mark.
    * 
    * Brackets the contents  in paragraph marks.
    * 
    * Asssumes currentPos on content. Compresses spacing, and handles inline marks. 
    * Stops on a linefeed.
    * 
    * Used for unmarked paragraphs. See `parse`. Used for post-mark paragraphs,
    * see `parsePostMarkParagraph`. Also used for block error recovery.
    */
  def parseUnmarkedParagraph()
  {
    println(s"parseUnmarkedParagraph currentChar: $currentChar")

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


  // ok, fix this
  def parseSideSignificant()
  {
    // in rough order of liklyhood?
    if (BlockBracketedMarks.contains(currentChar)) {
      // handles either mark and, for both, any following unsignificant paragraph
      // Short lookahead.
      // Need at least one extra non-whitespace char for an open
      // ...any whitespace is a close
      if (Character.isWhitespace(in(currentPos + 1))) {
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
            
          }
        }
      }
    }
  }
  
  /** Processes a string.
    *
    * Expects the line to contain LF  + EOF on the end.
    */
  private def parseMainLoop()
  {
    // Test for LF, or fail
    // Ensure the parser ends in the following seq of 3 chars.
    // NB: This parser makes no attempt to take the size of it's input,
    //  it defines limits bay setting control characters to the
    // end of input. In Java this can be expesive, as it is an
    // immutable string copy.
    // - LF
    // because most TML commands finish with LF
    // so this ensures trailing controls are completed.
    // - EOF
    // For the main loop detection
    // - EOF
    // The Literal block makes an unrigorous search for
    // the TML ending or EOF.
    // - this allows us to bump limit when processing forward
    // and also do an unchecked single forward for lookahead.

    errorIf(
      (in(in.size - 2) != LineFeed),
      s"String to parse (last - 2) must be LF in:'$in'"
    )
    /*
     errorIf(
     (in(in.size - 2) != EOF),
     s"String to parse (last - 2) must be TML EOF in:'$in'"
     )
     */
    // Test for EOF, or fail
    errorIf(
      (in(in.size - 1) != EOF),
      s"String to parse must end with TML EOF in:'$in'"
    )

    println(s"parsing: 'in' size:${in.size}")

    // this is ok to read '0', should always have EOF appended.
    resetCursor()


    while (currentChar != EOF) {

      // Jump whitespace for something useful
      // must allow to go to loop test again here, see if
      // limit reached, or spurious paragraph markup
      // will be printed on end of output.
      // NB: No spaces necessary or printed at block level
      //println(s"block level whitespace now: $currentChar")
      skipSpace()
      

      if (currentChar == LineFeed) {
        lineCount += 1
        currentLinePos = currentPos
        forward()
      }
      else {
        
        // Significant content
        // methods following should end on block level.

        // handle block marks, else treat as paragraph.
        //println(s"skipped whitespace lastMarkWasNewline:$lastMarkWasNewline now: $currentChar")
        if (SideSignificantMarks.contains(currentChar)) {
          parseSideSignificant()
        }
        else {
          // unmarked paragraph
          parseUnmarkedParagraph()
        }

      }
    }
  }

  /** Processes a line.
    *
    * The line should end in whitespace. For example, code which
    * reads from a file will often supply lines as strings ending
    * with a newline.
    *
    * The method throws an elidable exception if the line has no
    * terminating whitespace character. If required, the asserting
    * exception can be elided.
    *
    * @param line the string to be parsed for markup.
    */
  def parseLine(line: String)
  {
    // Test for LF, or fail
    errorIf(
      (line(line.size - 1) != LineFeed),
      s"String (representing line) to parse must end with LF line:'$line'"
    )
    
    in = line + EOF

    parseMainLoop()
  }

  /** Processes results of a string builder.
    *
    * In some circuemstances, this method may be faster than the other
    * methods, as the parser can take advantage of the string builder,
    * and not copy the string.
    * 
    * @param b the string builder containing the string to be parsed
    *  for markup.
    */
  def parseString(b: StringBuilder)
  {
    b += LineFeed
    b += EOF
    in = b.result()

    parseMainLoop()
  }
  
  /** Processes a string.
    *
    * @param str the string to be parsed for markup.
    */
  def parseString(str: String)
  {
    val t = System.currentTimeMillis()

    in = str  + LineFeed + EOF

    parseMainLoop()

    println(s"time: ${System.currentTimeMillis() - t}")
  }

  /** Attempts fixes on bracketing.
    *
    * Exhausts the mark stores, writing tags as necessary. Since TML
    * will always place marks it has detected onto stacks, this will
    * produce balanced markup. However, a final imbalance in the
    * stack implies some problem in markup or detection, and this
    * method can not protect against unintended or missed markup. But
    * the result will always have balanced tags.
    */
  def blockBalance(fix: Boolean)
  {
    // TODO: Inline is automatic, should always close?

    for (i <- 0 until inlineStack.size) {
      val d = inlineStack.head
      //val cm = controlMarkToString(controlMark)
      errLog.tailError(
        lineCount,
        99,
        d.startPos,
        s"Unclosed inline: '${d.attrs.control}${d.attrs.tagName}'"
      )

      if (fix) renderInlineClose(d.attrs)
      inlineStack.pop()
    }

    for (i <- 0 until blockStack.size) {
      val d = blockStack.head
      errLog.tailError(
        lineCount,
        99,
        d.startPos,
        s"Unclosed block: '${d.attrs.control}${d.attrs.tagName}'"
      )

      // Is ok, render and dispose of the markdata
      if (fix) renderBlockClose(d.attrs)
      blockStack.pop()

    }
  }

  /** Resets the parser to a starting condition.
    *
    * Clears the builder, stacks, errorlog, etc.
    */
  def clear()
  {
    currentChar = '\0'
    currentPos = 0
    blockStack.clear()
    paragraphMark = MarkData.textParagraph()
    inlineStack.clear()
    b.clear()
    errLog.clear()
    lineCount = 0
    currentLinePos = 0
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

  def apply(str: String)
  {
    println("running apply")
    clear()
    parseString(str)
    blockBalance(fix = false)
    println
    println(errLog.toText())
    println("out:")
    println(s"'${b.result}'")
    println(toString())
  }

 def result() : String = b.result()

}//Parser



object Parser {

  def apply()
      : Parser =
  {
    new HTML()
  }

}//Parser
