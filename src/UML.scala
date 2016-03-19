package tml


import java.util.regex.Matcher
import java.util.regex.Pattern

import scala.language.implicitConversions


/** Markup for creating unicode entities.
  *
  * 30 years on we still do not have keyboards or text processors
  * which handle extended character sets in any easy manner. Most
  * users are unaware of the existence of these codes, and unaware of
  * how to write them into a document.
  *
  * This set of methods is not designed to be the answer to the
  * problem. It is an easy-to-understand way of creating some common
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
  * example, they should not be used round pre-formed HTML pages where
  * &lt;script> or &lt;head> tags exist.
  *
  * HTML character entity references exist, but are unlikely to be as
  * compatible as Unicode, which usage stretches far beyond
  * browsers. Hexadecimal numeric character references are used in
  * official documentation, but rarely in practice. Greater
  * compatibility, and most currrent sources, recommend decimal
  * references. This markup parser uses decimal references.
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
        if (m.size == 1) "&#8208;"
        else {
          // em/dict else en
          if (m.size == 2) {
            // 'dictionary hyphen' else 'en'
            if (m(1) == '.')"&#8231;"
            else "&#8211;"
          }
          else "&#8212;"
        }
      }

      // ellipsis
      case '.' => "&#8230;"

      // legal marks
      case '(' => {
        m(1) match {
          case 'c' => "&#169;"
          case 't' => "&#8482;"
          case 'r' => "&#174;"
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
        if( m.size == 2) "&#8216"
        else "&#8217"
      }

      // double quotes
      case '"' => {
        // two quotes is open, one is close
        if( m.size == 2) "&#8220;"
        else  "&#8221;"
      }

      // guillemet open
      case '<' => "&#171;"

      // guillemet close
      case '>' => "&#187;"

      case ':' => {
        m match {
          case """:\A""" => "&#128;"
          case """:/A""" => "&#129;"
          case """:^A""" => "&#130;"
          case """::A""" => "&#132;"
          case """:oA""" => "&#133;"
          case """:cC""" => "&#199;"
          case """:\E""" => "&#200;"
          case """:/E""" => "&#201;"
          case """:^E""" => "&#202;"
          case """::E""" => "&#203;"
          case """:\I""" => "&#204;"
          case """:/I""" => "&#205;"
          case """:^I""" => "&#206;"
          case """::I""" => "&#207;"
          case """:\O""" => "&#210;"
          case """:/O""" => "&#211;"
          case """:^O""" => "&#212;"
          case """::O""" => "&#214;"
          case """:\U""" => "&#217;"
          case """:/U""" => "&#218;"
          case """:^U""" => "&#219;"
          case """::U""" => "&#220;"
          case """:\a""" => "&#224;"
          case """:/a""" => "&#225;"
          case """:^a""" => "&#226;"
          case """::a""" => "&#228;"
          case """:oa""" => "&#229;"
          case """:cc""" => "&#231;"
          case """:\e""" => "&#232;"
          case """:/e""" => "&#233;"
          case """:^e""" => "&#234;"
          case """::e""" => "&#235;"
          case """:\i""" => "&#236;"
          case """:/i""" => "&#237;"
          case """:^i""" => "&#238;"
          case """::i""" => "&#239;"
          case """:\o""" => "&#242;"
          case """:/o""" => "&#243;"
          case """:^o""" => "&#244;"
          case """::o""" => "&#246;"
          case """:\\u""" => "&#249;"
          case """:/u""" => "&#250;"
          case """:^u""" => "&#251;"
          case """::u""" => "&#252;"
          case """:/y""" => "&#253;"
          case """::y""" => "&#255;"
          case ":va" => "&#259;"

          case _ => {
            m(1) match {
              case '^' => m(2) + "&#770;"
              case 'u' => m(2) + "&#774;"
              case ':' => m(2) + "&#776;"
              case 'o' => m(2) + "&#778;"
              case 'v' => m(2) + "&#780;"
              case '\\' => m(2) + "&#768;"
              case '/' => m(2) + "&#769;"
              case 'c' => m(2) + "&#807;"
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


  private val mathSymbolSP = nc(""":m[xo-]""")
  private val fractionSP = nc("""[1235678]/[234568]""")

  private val mathP = Pattern.compile('(' + (
    mathSymbolSP
      | fractionSP
  ) + ')')

  private def maths(m: String)
      : String =
  {
    // also defined by m(1) == 'm'
    if(m(0) == ':') {
      m(2) match {
        // multiply, or dimensions
        case 'x' => "&#215;"
        // minus
        case '-' => "&#8722;"
        // degree
        case 'o' => "&#176;"
        case _ => m
      }

    }
    else {
      if(Character.isDigit(m(0))) {
        // fraction
        m match {
          case "1/2" => "&#189;"
          case "1/3" => "&#8531;"
          case "2/3" => "&#8532;"
          case "1/4" => "&#188;"
          case "3/4" => "&#190;"
          case "1/5" => "&#8533"
          case "2/5" => "&#8354"
          case "3/5" => "&#8535"
          case "4/5" => "&#8536"
          case "1/6" => "&#8537"
          case "5/6" => "&#8538"
          case "1/8" => "&#8539"
          case "3/8" => "&#8540;"
          case "5/8" => "&#8541;"
          case "7/8" => "&#8542;"
          case _ => m
        }
      }
      else m
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
