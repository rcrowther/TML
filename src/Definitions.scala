package tml

trait Definitions
{

  //---------------------
  // General definitions
  //---------------------

  // ASCII control code - ETX (end of text)
  protected val EOF: Char = 3
  protected val LineFeed: Char = 10
  protected val Space: Char = 32

  //val CarriageReturn: Char = 13

  // What is this? Is a backslash.
  //val EscapeChar: Char = 92
  // Newline11
  // Unix = LF
  // Win/ASCII = CR ~ LF
  //val CarrageReturn = 13
}
