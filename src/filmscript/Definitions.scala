package tml
package filmscript

trait Definitions
{

  this : Parser =>

  val blockBracketedTagnameAliases = Map.empty[String, String]

  val BlockBracketedMarks = Map(
    // on-screen text
    '>' -> "",
    // simutaneous action
    '|' -> ""
  )

  val BlockBracketedTitledMarks = Seq(
    // Title
    '?',
    // scene
    '=',
    // character
    '#',
    // shotseries
    '+',
    // shot
    '-'
  )

  val BlockParagraphDefaultedMarks: Map[Char, String] = Map.empty

  override val BlockBracketedLiteralMark: Char = '\u0000'

  val BlockParagraphNoDefaultMarks: Seq[Char] = Seq(
    // transition
    '~',
    // instruction
    '*',
    //shot
    '@'
  )


  val InlineMarkDefault: String = ""

  val inlineBracketedTagnameAliases: Map[String,String] = Map.empty


  override val BlockSelfClosingMark: Char = '\u0000'
  val BlockSelfClosingMarkDefault = ""


  override val InlineSelfClosingMark: Char = '\u0000'
  val InlineSelfClosingMarkDefault: String = ""

}//Definitions
