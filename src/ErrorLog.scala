package tml



/** Stores errors for print output.
  */
final class ErrorLog {

  private val warnings = new collection.mutable.ArrayBuffer[Error]
  private val errors = new collection.mutable.ArrayBuffer[Error]
  private val tailErrors = new collection.mutable.ArrayBuffer[Error]


  def clear()
  {
    errors.clear()
    tailErrors.clear()
    warnings.clear()
  }

  def warning(
    lineNum: Int,
    linePos: Int,
    pos: Int,
    message: String
  )
  {
    warnings += new Error(lineNum, linePos, pos, message)
  }

  def +=(
    lineNum: Int,
    linePos: Int,
    pos: Int,
    message: String
  )
  {
    errors += new Error(lineNum, linePos, pos, message)
  }

  def tailError(
    lineNum: Int,
    linePos: Int,
    pos: Int,
    message: String
  )
  {
    tailErrors += new Error(lineNum, linePos, pos, message)
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
        b append e.lineNum
        b ++= ":"
        b append e.lineRelativePos
        b ++= " error: "
        b ++= e.message
        b ++= "\n"
      }

      b += '\n'
    }

    if (!warnings.isEmpty) {
      warnings.foreach { e =>
        b append e.lineNum
        b ++= ":"
        b append e.lineRelativePos
        b ++= " warning: "
        b ++= e.message
        b ++= "\n"
      }

      b += '\n'
    }

    if (!tailErrors.isEmpty) {
      tailErrors.foreach { e =>
        //b append e.lineNum
        //b ++= ":"
        //b append e.lineRelativePos
        b ++= " tail error: "
        b ++= e.message
        b ++= "\n"
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
