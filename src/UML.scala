package tml


import java.util.regex.Matcher
import java.util.regex.Pattern

import scala.language.implicitConversions


/** Markup for creating inaccessible unicode codepoints.
  *
  * 30 years on we still do not have keyboards or text processors
  * which handle extended character sets in any easy manner. Most
  * users are unaware of the existence of these codes, and unaware of
  * how to write them into a document.
  *
  * This set of methods is not designed to be the answer to the
  * problem. It is an easy-to-understand way of creating common
  * codepoints, mostly using keyboard-available characters.
  *
  * Like any conversion methods for typography, these methods will
  * make mistakes. The hope is only that the improvement outweighs
  * the mistakes.
  *
  * The markup depends heavily on the COLON `:`, followed by other
  * single-character codes.
  *
  * These are raw methods, which do not defend against much. For
  * example, the code may damage pre-formed HTML pages where
  * &lt;script> or &lt;head> tags exist.
  *
  * Java strings are encoded as UTF-16, and string handling mostly
  * works in UTF-16. Thus this Scala implementation of UML does not
  * use entities, but punches in the necessary codepoints. Please bear
  * in mind that the results of a parse, even if correct, may display,
  * in display code not handling UTF-16, as distorted.
  *
  * [[http://alistapart.com/article/emen]]
  *
  * ==Alternatives==
  * Smartypants
  * [[http://daringfireball.net/projects/smartypants/]]
  */
//TODO: soft hyphen (&#173;
//TODO: Make into one pass?
object UML {

  // Utility and implicits
  // making regex a little easier on the eye. A little...
  private def nc(s: String) : String = "(?:" + s + ')'

  private[this] class RegexConcat(s: String) {
    def |(r: String): String = { s + '|' + r }
  }

  private[this] implicit def string2RegexConcat(s: String)
      : RegexConcat = new RegexConcat(s)




  //-------
  // Basic
  //-------


  private val elipsisSP = nc("""\.\.\.""")
  private val hyphenDashSP = nc("""-(?:-{1,2}|\.)?""")
  private val legalMarkSP = nc("""\([ctr]\)""")

  private val basicP = Pattern.compile('(' + (
    hyphenDashSP
      | elipsisSP
      | legalMarkSP
  ) + ')')

  //println(s"basicP $basicP")

  private def basics(m: String)
      : String =
  {

    m(0) match {

      // dashes
      case '-' => {
        //println(s"dash match $m")
        // hyphen else dash
        if (m.size == 1) "\u2010"
        else {
          // em/dict else en
          if (m.size == 2) {
            // 'dictionary hyphen' else 'en'
            if (m(1) == '.')"\u2027"
            else "\u2013"
          }
          else "\u2014"
        }
      }

      // ellipsis
      case '.' => "\u2026"

      // legal marks
      case '(' => {
        m(1) match {
          case 'c' => "\u00A9"
          case 't' => "\u2122"
          case 'r' => "\u00AE"
        }
      }

      // source match, but no recognition match
      case _ => {
        println(s"unmatched search? $m")
        m
      }
    }
  }




  //-------------
  // Typographic
  //-------------


  private val doubleQuoteSP = """(?:""?)"""
  private val singleQuoteSP = """(?:''?)"""
  private val guillemetOpenSP = """(?:\<\<)"""
  private val guillemetCloseSP = """(?:>>)"""
  
  private val accentSP = nc(""":[:c^vuo/\\][A-Za-z]""")

  private val typographyP = Pattern.compile('(' + (
    singleQuoteSP
      | doubleQuoteSP
      | guillemetOpenSP
      | guillemetCloseSP
      | accentSP
  ) + ')')

  //println(s"typographyP $typographyP")
  private def typographies(m: String)
      : String =
  {

    m(0) match {
      // single quotes
      case '\'' => {
        // two quotes is open, one is close
        if( m.size == 2) "\u2018"
        else "\u2019"
      }

      // double quotes
      case '"' => {
        // two quotes is open, one is close
        if( m.size == 2) "\u201C"
        else  "\u201D"
      }

      // guillemet open
      case '<' => "\u00AB"

      // guillemet close
      case '>' => "\u00BB"

      case ':' => {
        m match {
          case """:\A""" => "\u00C0"
          case """:/A""" => "\u00C1"
          case """:^A""" => "\u00C2"
          case """::A""" => "\u00C4"
          case """:oA""" => "\u00C5"
          case """:cC""" => "\u00C7"
          case """:\E""" => "\u00C8"
          case """:/E""" => "\u00C9"
          case """:^E""" => "\u00CA"
          case """::E""" => "\u00CB"
          case """:\I""" => "\u00CC"
          case """:/I""" => "\u00CD"
          case """:^I""" => "\u00CE"
          case """::I""" => "\u00CF"
          case """:\O""" => "\u00D2"
          case """:/O""" => "\u00D3"
          case """:^O""" => "\u00D4"
          case """::O""" => "\u00D6"
          case """:\U""" => "\u00D9"
          case """:/U""" => "\u00DA"
          case """:^U""" => "\u00DB"
          case """::U""" => "\u00DC"
          case """:\a""" => "\u00E0"
          case """:/a""" => "\u00E1"
          case """:^a""" => "\u00E2"
          case """::a""" => "\u00E4"
          case """:oa""" => "\u00E5"
          case """:cc""" => "\u00E7"
          case """:\e""" => "\u00E8"
          case """:/e""" => "\u00E9"
          case """:^e""" => "\u00EA"
          case """::e""" => "\u00EB"
          case """:\i""" => "\u00EC"
          case """:/i""" => "\u00ED"
          case """:^i""" => "\u00EE"
          case """::i""" => "\u00EF"
          case """:\o""" => "\u00F2"
          case """:/o""" => "\u00F3"
          case """:^o""" => "\u00F4"
          case """::o""" => "\u00F6"
          case ":\\u" => "\u00F9"
          case """:/u""" => "\u00FA"
          case """:^u""" => "\u00FB"
          case """::u""" => "\u00FC"
          case """:/y""" => "\u00FD"
          case """::y""" => "\u00FF"
            //case ":ua" => "\u0103"

          case _ => {
            m(1) match {
              case '^' => m(2) + "\u0302"
              case 'u' => m(2) + "\u0306"
              case ':' => m(2) + "\u0308"
              case 'o' => m(2) + "\u030A"
              case 'v' => m(2) + "\u030C"
              case '\\' => m(2) + "\u0300"
              case '/' => m(2) + "\u0301"
              case 'c' => m(2) + "\u0327"
              case _ => m
            }
          }
        }
      }
      case _ => {
        println(s"unmatched search? $m")
        m
      }
    }
  }




  //------
  // Math
  //------

  private val mathSymbolSP = nc(""":(?:(m[xo-])|([12345678][234568]))""")

  private val mathP = Pattern.compile('(' + (
    mathSymbolSP
  ) + ')')

  private def maths(m: String)
      : String =
  {
    m(1) match {
      case 'm' => {
        m(2) match {
          // multiply, or dimensions
          case 'x' => "\u00D7"
          // minus
          case '-' => "\u2212"
          // degree
          case 'o' => "\u00B0"
          case _ => m
        }
      }
      case _ => {
        // fractions
        m(1) match {
          case '1' => {
            m(2) match {
              case '2' => "\u00BD"
              case '3' => "\u2153"
              case '4' => "\u00BC"
              case '5' => "\u2155"
              case '6' => "\u2159"
              case '8' => "\u215B"
              case _ => m
            }
          }

          case '2' => {
            m(2) match {
              case '3' => "\u2154"
              case '5' => "\u2156"
              case _ => m
            }
          }

          case '3' => {
            m(2) match {
              case '4' => "\u00BE"
              case '5' => "\u2157"
              case '8' => "\u215C"
              case _ => m
            }
          }

          case '4' => {
            m(2) match {
              case '5' => "\u2158"
              case _ => m
            }
          }

          case '5' => {
            m(2) match {
              case '6' => "\u215A"
              case '8' => "\u215D"
              case _ => m
            }
          }

          case '7' => {
            m(2) match {
              case '8' => "\u215E"
              case _ => m
            }
          }
          case _ => m
        }

      }
    }

  }



  //---------
  // General
  //---------

  // unused?
  private def stringReplaceByMatcher(
    p: Pattern,
    repCallback: (Matcher) => String,
    str: String
  )
      : String =
  {
    var b = new StringBuffer()
    var currStr = ""
    // Dash/Hyphens
    // We test each side for space, so need to put it back.
    val m = p.matcher(str)
    while (m.find()) {
      m.appendReplacement(b, repCallback(m))
    }
    m.appendTail(b)
    b.toString()
  }

  /** Replaces matches, on group 1, in a string by applying a function.
    */
  private def stringReplace(
    p: Pattern,
    repCallback: (String) => String,
    str: String
  )
      : String =
  {
    var b = new StringBuffer()

    val m = p.matcher(str)
    while (m.find()) {
      m.appendReplacement(b, repCallback(m.group(1)))
    }
    m.appendTail(b)
    b.toString()
  }

  private def stringReplace(
    p: Pattern,
    replacement: String,
    str: String
  )
      : String =
  {
    val m = p.matcher(str)
    m.replaceAll(replacement)
  }



  //----------------
  // API
  //---------------


  // LETTER ~ '-' Hyphen, word separation


  /** Simple set of encodings.
    *
    * Legal marks, 
    *
    * {{{
    * "(c)" = copyright
    * "(t)" = trademark
    * "(r)" = registered
    * }}}
    *
    * Hyphens, dashes,
    *
    * {{{
    * "---" = em for new material, or sentence joining.
    * "--" = en for ranges.
    * "-." = dictionary hyphen.
    * "-" = hyphen.
    * }}}
    *
    * Ellipse,
    *
    * {{{
    * "..." = ellipsis for missiung text, including tailing off.
    * }}}
    *
    * These encodings are supported on many systems.
    */
  def basic(str: String)
      : String =
  {
    stringReplace(basicP, basics _, str)
  }

  /** Typographical set of encodings.
    *
    * Quotes, 
    *
    *  {{{
    * "''" single quotes open
    * "'" single quote close, and apostrophe
    * "\"\""  double quote open 
    * "\"" double quote close
    * "<<" guillemet open
    * ">>" guillemet close
    * }}}
    *
    * Combining marks (accents), 
    *
    * {{{
    * ':' ~ oneOf(
    * c CEDILLA
    * : DIAERESIS
    * u BREVE
    * ^ CIRCUMFLEX
    * v CARRON
    * o RING
    * \ GRAVE ACCENT
    * / ACUTE ACCENT
    * ) ~ LETTER
    * }}}
    *
    * The combining mark tables try to create a character, then fall
    * back to using combining characters.
    *
    * Also the basic set, see `basic` comments.
    *
    * Uses some (well-known) high-numeric encodings.
    */
  def typography(str: String)
      : String =
  {
    basic(stringReplace(typographyP, typographies _, str))
  }


  /** Mathematical encodings.
    *
    * Fractions,
    * {{{
    * DIGIT ~ '/' ~ DIGIT
    * }}}
    *
    * Mathematical multiply/minus, and degree.
    *
    * {{{
    * ":mx" = multiply
    * ":m-" = minus
    * ":mo" = degree
    * }}}
    *
    * Uses some high-numeric encodings.
    */
  def math(str: String)
      : String =
  {
    stringReplace(mathP, maths _, str)
  }

  /** Attempt all UML encodings.
    *
    * May use some high-numeric encodings.
    */
  def apply(str: String)
      : String =
  {
    // Math must come before typography
    // (and it's rather flakey hyphen)
    typography(math(str))
  }

  /** Attempt all UML encodings.
    *
    * May use some high-numeric encodings.
    */
  def apply(ts: Traversable[String])
      : Traversable[String] =
  {
    // Math must come before typography
    // (and it's rather flakey hyphen)
    ts.map(s => typography(math(s)))
  }

}//UML
