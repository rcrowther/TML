package tml



/** Stores errors for print output.
  */
final class ErrorLog
    extends Definitions
{

  private val warnings = new collection.mutable.ArrayBuffer[Error]
  private val errors = new collection.mutable.ArrayBuffer[Error]
  private val tailErrors = new collection.mutable.ArrayBuffer[Error]





  //-----------------
  // Utility render
  //-----------------

  /** Represents a control char as a string.
    *
    * Converts whitespace into something human-readable.
    */
  private def controlMarkToString(controlMark: Char)
      : String =
  {
    if (controlMark == ' ') {
      "<space>"
    }
    else {
      if (controlMark == LineFeed) {
        "<linefeed (10)>"
      }
      else {
        if (Character.isWhitespace(controlMark)) {
          s"<whitespace (${controlMark.toInt})>"
        }
        else controlMark.toString
      }
    }
  }

  /** Appends a represention of the original position and display of a mark.
    */
  private def addOpeningBlockString(b: StringBuilder, e: Error)
      : Boolean =
  {
    val mdO = e.openingMarkData

    if(mdO != None) {
      val md = mdO.get
      b ++= "(opened at "
      b append md.lineNum
      b += ':'
      b append (md.pos - md.linePos)

      b ++= " '"
      b append md.control
      b append md.tagName
      b ++= "')"
      true
    }
    else false
  }

  private def addAdviceString(b: StringBuilder, e: Error)
      : Boolean =
  {
    val ok = !e.advice.isEmpty
    if (ok) {
      b ++= e.advice
      // in case followed by other information
      b += ' '
    }
    ok
  }



  //-----------------
  // Specific errors
  //-----------------

  // too close to bother with mark data
  def textAttributeClosedByNewline(
    it: InputIterator
  )
  {
    warnings += Error(
      it,
      "Text attribute closed by newline",
      "unintended?"
    )
  }

  def urlAttributeClosedByNewline(
    it: InputIterator
  )
  {
    warnings += Error(
      it,
      "URL attribute closed by newline",
      "unintended?"
    )
  }

  def unknownAttributeMark(
    it: InputIterator,
    cm: Char
  )
  {
    errors += Error(
      it,
      s"Unknown mark in attribute list: mark: '${controlMarkToString(cm)}'",
      "trying forward"
    )
  }

  def emptyBlockStack(
    it: InputIterator,
    cm: Char
  )
  {
    errors += Error(
      it,
      s"Block close but stack is empty: mark: '${controlMarkToString(cm)}'",
      "mark is ignored"
    )
  }

  def misMatchBlockStack(
    it: InputIterator,
    openMark: MarkData,
    cm: Char
  )
  {
    errors += Error(
      it,
      openMark,
      s"Block close does not match mark on stack: mark '${controlMarkToString(cm)}'",
      "mark is ignored"
    )
  }

  def emptyInlineStack(
    it: InputIterator
  )
  {
    errors += Error(
      it,
      s"Inline close mark but stack empty",
      "mark is ignored"
    )
  }

  def overrunInlineLiteral(
    openMark: MarkData
  )
  {
    errors += Error.positionByMarkdata(
      openMark,
      s"Literal Block reached EOF without close",
      "stopping literal"
    )
  }

  def inlineClosedByNewline(
    it: InputIterator,
    openMark: MarkData
  )
  {
    warnings += Error(
      it,
      openMark,
      s"Inline mark closed by newline",
      "unintended?"
    )
  }


  def unclosedBlock(
    openMark: MarkData
  )
  {
    tailErrors += Error.positionByMarkdata(
      openMark,
      "Unclosed block",
      "auto-closed"
    )
  }



  //---------------
  // Stock methods
  //---------------

  def clear()
  {
    errors.clear()
    tailErrors.clear()
    warnings.clear()
  }

  def isEmpty
      : Boolean =
  {
    (errors.isEmpty && warnings.isEmpty && tailErrors.isEmpty)
  }


  def toText()
      : String =
  {
    val b = new StringBuilder()


    if (!errors.isEmpty) {
      errors.foreach { e =>
        e.addPosString(b)
        b ++= " error: "
        b ++= e.message
        b ++= "\n"
        if (addAdviceString(b, e) ||  addOpeningBlockString(b, e))  b ++= "\n"
      }

      b += '\n'
    }

    if (!warnings.isEmpty) {
      warnings.foreach { e =>
        e.addPosString(b)
        b ++= " warning: "
        b ++= e.message
        b ++= "\n"
        if (addAdviceString(b, e) ||  addOpeningBlockString(b, e))  b ++= "\n"

      }

      b += '\n'
    }

    if (!tailErrors.isEmpty) {
      tailErrors.foreach { e =>
        e.addPosString(b)
        b ++= " tail error: "
        b ++= e.message
        b ++= "\n"
        // No opening blocks on tail errors, they ARE the opening block.
        if (addAdviceString(b, e))  b ++= "\n"
      }

      b += '\n'
    }

    if (!errors.isEmpty) {
      b append errors.size
      b ++= " error(s) found\n"
    }
    if (!warnings.isEmpty) {
      b append warnings.size
      b ++= " warning(s) found\n"
    }
    if (!tailErrors.isEmpty) {
      b ++= "balance errors found!\n"
      b += '\n'
    }

    b.result
  }

}//ErrorLog
