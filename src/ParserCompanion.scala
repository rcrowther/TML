package tml



trait ParserCompanion[PARSER <: Parser]
{

  /** Creates an instance of the parser as builder.
    */
  def builder(ot: OutputTarget): PARSER

  /** Apply input, fixes errors, no error reporting.
    *
    * Default behaviour in Spec.
    *
    * The output target is not cleared, to allow for building.
    * Clearing may be necessary for results, especially when using
    * streams.
    */
  def apply(ot: OutputTarget, it: InputIterator)
  {
    val p = builder(ot)
    p(it)
    p.blockBalance(fix = true)
    //println("out:")
    //println(s"'${p.result()}'")
    //ot.clear()
  }

  /** Apply input, fixes errors, no error reporting.
    *
    * Shortcut to return a string from any `InputIterator`.
    */
  def apply(it: InputIterator)
      : String =
  {
    val b = new StringBuilder()
    val p = builder(b)
    p(it)
    p.blockBalance(fix = true)
    b.result()
  }

  /** Apply input, ignoring errors, with error reporting.
    *
    * The output target is not cleared, to allow for building.
    * Clearing may be necessary for results, especially when using
    * streams.
    */
  def errorReport(ot: OutputTarget, it: InputIterator)
  {
    val p = builder(ot)
    p.logger = new ActiveLogger()
    p(it)
    p.blockBalance(fix = false)
    println(p.logger.toText())
    //ot.clear()
  }

  /** Apply input, ignoring errors, with error reporting.
    *
    * Shortcut to return a string from any `InputIterator`.
    */
  def errorReport(it: InputIterator)
      : String =
  {
    val b = new StringBuilder()
    val p = builder(b)
    p.logger = new ActiveLogger()
    p(it)
    p.blockBalance(fix = false)
    println(p.logger.toText())
    b.result()
  }

  /** Apply input, not fixing errors, with status reporting.
    */
  /*
   def profile(it: InputIterator)
   {
   val p = builder()
   p(it)
   p.blockBalance(fix = false)
   println("out:")
   println(s"'${p.result()}'")
   println
   println(p.logger.toText())
   }
   */

}//ParserCompanion
