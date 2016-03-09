package tml


import java.util.regex.Matcher
import java.util.regex.Pattern




/** Markup for creating unicode entities.
  *
  * 30 years on we still do not have keyboards or text processors
  * which handle extended character sets in any easy manner. Most
  * users are unaware of the existence of these codes, and unaware of
  * how to write them into a document.
  *
  * This set of methods is not designed to be the answer to the
  * problem. It is an easy-to-understand way of creating some
  * common codepoints, mostly using keyboard-available
  * characters.
  *
  * Like any conversion methods for typography, these methods will
  * make mistakes. The hope is only that the improvement outweighs
  * the mistakes.
  *
  * These are raw methods, which do not defend against much. For
  * example, they should not be used round pre-formed HTML pages where
  * &lt;script> or &lt;head> tags exist.
  *
  * ==Alternatives==
  * Smartypants
  * [[http://daringfireball.net/projects/smartypants/]]
  */
object UML {

  private val elipsisP = Pattern.compile("""\.\.\.""")

  //Guillemets
  private val guillemetOpenP = Pattern.compile("""\<\<""")
  private val guillemetCloseP = Pattern.compile(""">>""")

  // Quotes
  private val doubleQuoteOpenP = Pattern.compile("""``""")
  private val doubleQuoteCloseP = Pattern.compile("""''""")

  // math
  // Maths x, or dimentions
  private val mathMultiplyP = Pattern.compile(""":mx""")
  // Maths '-'
  private val mathMinusP = Pattern.compile(""":m-""")
  // Math or temperature degree
  private val mathDegreeP = Pattern.compile(""":mo""")

  private val fractionP = Pattern.compile("""([1235678]/[234568])""")

  private def fraction(m: String): String = {
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

  private val ligatureP = Pattern.compile("""([AOaof]\|[Eefi])""")

  private def ligature(m: String): String = {
    m match {
      case "a|e" => "&#230;"
      case "A|E" => "&#198;"
      case "o|e" => "&#339;"
      case "O|E" => "&#338;"
      case "f|f" => "&#64256;"
      case "f|i" => "&#64257;"
      case _ => m
    }
  }

  private val accentP = Pattern.compile("""(:[:c^vuo/\\][A-Za-z])""")

  // TODO: A fair few more accents?
  private def accent(m: String): String = {
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

  private val dashP = Pattern.compile("""(-(?:(?:-{1,2})|(?:\.)))""")
  private val hyphenP = Pattern.compile("""([A-Za-z])-""")

  private def dash(m: String): String = {
    m match {
      case "---" => "&#8212;"
      case "--" => "&#8211;"
      case "-." => "&#8231;"
      case _ => "-"
    }
  }

  //&#8201 #hairspace
  private val legalMarkP = Pattern.compile("""\((c|tm|r)\)""")

  private def legalMark(m: String): String = {
    m match {
      case "c" => "&#169;"
      case "tm" => "&#8482;"
      case "r" => "&#174;"
    }
  }

  /*
   UML.replaceLegalMarks(" (c)")
   UML.replaceLegalMarks(" (tm)")
   UML.replaceLegalMarks(" -. ")
   UML.replaceLegalMarks(" --- ")

   */

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

  private def stringReplace(
    p: Pattern,
    repCallback: (String) => String,
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

  def legalMarks(str: String): String =
    stringReplace(legalMarkP, legalMark _, str)

  /** Combining marks
    *
    * Also often 'accents'
    *
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
    *
    * The tables attempt to match a character first. Currently
    * implemented is most of 'Latin-1 Supplement' Letters, meaning
    * vowels (including 'y') for accents not including BREVE and
    * CARRON.
    *
    * The tables fall back to using combining characters.
    *
    * Java advice: "never build your own tables".
    */
  def combiningMarks(str: String): String =
    stringReplace(accentP, accent _, str)


  /** Dashes
    *
    * '...' ellipsis used for
    * LETTER ~ '-' Hyphen (must have letters on both sides) word separation
    * '--' En used for ranges
    * '---' Em, used for
    * '-.' Hyphenation Point used for
    */
  def dashesEllipsis(str: String)
      : String =
  {
    stringReplace(hyphenP, "$1&#8208;",
      stringReplace(dashP, dash _,
        stringReplace(elipsisP, "&#8230;", str)
      ))
  }

  /** Quotes
    *
    * '``' and '''' double quotes
    * '<<' and '>>' guilemets
    */
  def quotes(str: String)
      : String =
  {
    stringReplace(doubleQuoteOpenP, "&#8220;",
      stringReplace(doubleQuoteCloseP, "&#8221;",
        stringReplace(guillemetOpenP, "&#171;",
          stringReplace(guillemetCloseP, "&#187;", str)
        )))
    //stringReplace(singleQuoteP, "&#8230", str)
  }

  /** Ligatures.
    *
    * Note that Unicode regards ligatures as a feature of
    * presentation, so discourage their use. They should be used only
    * when clearly common and expected.
    *
    * LETTER ~ '|' ~ LETTER
    */
  def ligatures(str: String): String =
    stringReplace(ligatureP, ligature _, str)

  /** Fractions.
    *
    * All non-reducable fractions with factorial 2,3,4,5,7,8.
    * Non-reducable means 3/4 exists, but not 2/4 (reduces to 1/2)
    *
    * DIGIT ~ '/' ~ DIGIT
    */
  def fractions(str: String): String =
    stringReplace(fractionP, fraction _, str)

  /** Simple set of encodings.
    *
    * Legal marks, dashes and elipses.
    *
    * This has the advantage of only using low-numeric encodings
    * which should work on many systems.
    */
  def basic(str: String)
      : String =
  {
    dashesEllipsis(legalMarks(str))
  }

  /** Typographical set of encodings.
    *
    * Quotes, accents, ligatures, dashes, ellipsis,
    * and legal marks.
    *
    * Only uses low-numeric encodings
    * which should work on any system.
    */
  def typography(str: String)
      : String =
  {
    quotes(combiningMarks(ligatures(basic(str))))
  }

  /** Mathematical encodings.
    *
    * Fractions, mathematical multiply/minus, and degree.
    *
    * Uses some higher-numeric encodings.
    */
  def math(str: String)
      : String =
  {
    fractions(
      // 'x', or dimensions
      stringReplace(mathMultiplyP, "&#215;",
        // '-'
        stringReplace(mathMinusP, "&#8722;",
          // degree
          stringReplace(mathDegreeP, "&#176;", str)
        )))
  }

  /** Attempt all UML encodings.
    *
    * Uses some higher-numeric encodings.
    */
  def apply(str: String)
      : String =
  {
    // Math must come before typography
    // (and it's rather flakey hyphen)
    typography(math(str))
  }

}//UML
