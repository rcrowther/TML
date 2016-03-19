package tml

trait ParserCompanion[PARSER <: Parser]
 {

/** Creates an instance of the parser for building.
*/
def builder(): PARSER

/** Apply input, fixing errors, no error reporting.
*
* Default behaviour in Spec.
*/
  def apply(it: InputIterator)
  {
    val p = builder()
    p(it)
    p.blockBalance(fix = true)
    println("out:")
    println(s"'${p.result()}'")
}

/** Apply input, fixing errors, with error reporting.
*/
  def errorReport(it: InputIterator)
  {
    val p = builder()
p.logger = new ActiveLogger()
    p(it)
    p.blockBalance(fix = true)
    println("out:")
    println(s"'${p.result()}'")
    println
    println(p.logger.toText())
}

/** Apply input, not fixing errors, with status reporting.
*/
/*
  def profile(it: InputIterator)
  {
    val p = builder()
    p(it)
    p.blockBalance(fix = true)
    println("out:")
    println(s"'${p.result()}'")
    println
    println(p.logger.toText())
}
*/

}//ParserCompanion
