package tml



/** Stores errors for print output.
  */
trait Logger
    extends Definitions
{

  //-----------------
  // General errors
  //-----------------

  /** Log a warning from a renderer.
    */
  def rendererWarning(
    it: InputIterator,
    message: String,
    advice: String
  )


  /** Log an error from a renderer.
    */
  def rendererError(
    it: InputIterator,
    message: String,
    advice: String
  )



  //-----------------
  // Specific errors
  //-----------------

  def bracketAttributeClosedByNewline(
    it: InputIterator
  )

  def emptyBlockStack(
    it: InputIterator,
    cm: Char
  )

  def misMatchBlockStack(
    it: InputIterator,
    openMark: MarkData,
    cm: Char
  )

  def emptyInlineStack(
    it: InputIterator
  )

  def overrunInlineLiteral(
    openMark: MarkData
  )

  def inlineClosedByNewline(
    it: InputIterator,
    openMark: MarkData
  )


  def unclosedBlock(
    openMark: MarkData
  )




  //---------------
  // Stock methods
  //---------------

  def clear()

  def isEmpty
      : Boolean

  def toText()
      : String

}//Logger



object Logger{

  /** a logger which does nothing.
    */
  private[this] val inactiveThing : Logger = new Logger {
    def bracketAttributeClosedByNewline(it: tml.InputIterator): Unit = {}
    def emptyBlockStack(it: tml.InputIterator,cm: Char): Unit = {}
    def emptyInlineStack(it: tml.InputIterator): Unit = {}
    def inlineClosedByNewline(it: tml.InputIterator,openMark: tml.MarkData): Unit = {}
    def misMatchBlockStack(it: tml.InputIterator,openMark: tml.MarkData,cm: Char): Unit = {}
    def overrunInlineLiteral(openMark: tml.MarkData): Unit = {}
    def rendererError(it: tml.InputIterator,message: String,advice: String): Unit = {}
    def rendererWarning(it: tml.InputIterator,message: String,advice: String): Unit = {}
    def unclosedBlock(openMark: tml.MarkData): Unit = {}

    def clear(): Unit = {}
    def isEmpty: Boolean = true
    def toText(): String = "logger inactive"
  }

  def inactive() : Logger = inactiveThing

}//Logger
