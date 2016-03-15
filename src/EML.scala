package tml

import java.util.regex.Matcher
import java.util.regex.Pattern



/** Escape html markup.
  *
  * This method is for display of HTML markings which a browser would
  * interprete as code.
  *
  * The method is provided for administrator provided/preprocessed
  * data which needs to be displayed. Do *NOT* use this method for
  * untrusted data. It will allow XSS attacks on other sites.
  *
  * The method is not guaranteed to produce intended output. No
  * unparsed method could (the method uses regex). However, it makes
  * some efforts to,
  *  - avoid encoding existing entity ampersands
  *  - detect HTML tag angle brackets (so, not all angle brackets)
  * The method also encodes quotation marks, as defined by WC3.
  *
  * Many entities exist above &#160. This method only translates
  * markup-significant, low numeric, entities.
  *
  * For WC3 entities
  * [[https://www.w3.org/TR/REC-html40/sgml/entities.html]]
  *
  * For insecure data
  * [[https://www.owasp.org/index.php/XSS_%28Cross_Site_Scripting%29_Prevention_Cheat_Sheet]]
  *
  * Also try
  * [[https://code.google.com/p/owasp-esapi-java/source/browse/trunk/src/main/java/org/owasp/esapi/codecs/HTMLEntityCodec.java]]
  */
// tml.EML("&")
// tml.EML("<div>")
// tml.EML.defensive("</div>")
// tml.EML("""<div id="top">""")
// tml.EML("<duff>")
// tml.EML("&")
// tml.EML.htmlToDisplayHTML("""<div id="top">""", true)
// tml.EML("&#x19")
// tml.EML(tml.FileReader("""/home/rob/Code/scala/TML/text/SPEC"""))
//  tml.HTML(tml.EML(tml.FileReader("""/home/rob/Code/scala/TML/text/SPEC""")))
object EML {

  // Utility and implicits
  // making regex a little easier on the eye. A little...
  private def nc(s: String) : String = "(?:" + s + ')'

  private[this] class RegexConcat(s: String) {
    def |(r: String): String = { s + '|' + r }
  }

  private[this] implicit def string2RegexConcat(s: String)
      : RegexConcat = new RegexConcat(s)

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
 // println(s" mr ${m.toMatchResult}")
 // println(s" gc ${m.groupCount()}")

      m.appendReplacement(b, repCallback(m.group(1)))
    }
    m.appendTail(b)
    b.toString()
  }



  // ?! = negative lookahead, so fails if present
  // so fails if following text matches
  // optional(entity marks) ~ (Hexlike number | word character) ~ ;
  // i.e. looks like an HTML entity
  private val ampToEntityDefensiveSP = nc("""(&)(?!#?[xX]?(?:[0-9a-fA-F]+|\w+);)""")


  private val htmlTagsSP = nc("""[A-Za-z]""")

  // "<" ~ ([lookahead] ~ optional(/) ~ Letter)
  private val lbToEntityDefensiveSP = nc("""(<)(?=/?[A-Za-z])""")

  // ([lookback] Letter | "\"") ~ ">"
  private val rbToEntityDefensiveSP = nc("""(?<=[A-Za-z"])>""")

  private val defensiveP = Pattern.compile( '(' + (
     ampToEntityDefensiveSP
      | lbToEntityDefensiveSP
      | rbToEntityDefensiveSP
  ) + ')')

  private val simpleP = Pattern.compile("""([&<>])""")

  //println(s"defensiveP: $defensiveP")
  //println(s"htmlCloseTagP: $htmlCloseTagP")
  //println(s"ampToEntitySP: $ampToEntitySP")


  private def stringToNumericEntity(s: String)
      : String =
  {

    s match {
      case "&" => "&#26;"
      case "<" => "&#60;"
      case ">" => "&#62;"

      // source match, but no recognition match
      case _ => {
        println(s"unmatched search? $s")
        s
      }
    }
  }


  private def stringToNamedEntity(s: String)
      : String =
  {

    s match {
      case "&" => "&#amp;"
      case "<" => "&#lt;"
      case ">" => "&#gt;"
      //        case '"' => b ++= "&#quot;"
      // source match, but no recognition match
      case _ => {
        println(s"unmatched search? $s")
        s
      }
    }
  }

  // htmlToDisplay
  def defense(str: String, asNames: Boolean)
      : String =
  {
    if (asNames) stringReplace(defensiveP, stringToNamedEntity _, str)
    else stringReplace(defensiveP, stringToNumericEntity _, str)
  }

  def defensive(str: String)
      : String =
  {
    stringReplace(defensiveP, stringToNamedEntity _, str)
  }
  def defensive(st: Traversable[String])
      : Traversable[String] =
  {
    st.map{ s =>
    stringReplace(simpleP, stringToNamedEntity _, s)
}
  }
  def apply(str: String, asNames: Boolean)
      : String =
  {
    if (asNames) stringReplace(simpleP, stringToNamedEntity _, str)
    else stringReplace(simpleP, stringToNumericEntity _, str)
  }
  def apply(str: String)
      : String =
  {
    stringReplace(simpleP, stringToNamedEntity _, str)
  }
  def apply(st: Traversable[String])
      : Traversable[String] =
  {
    st.map{ s =>
    stringReplace(simpleP, stringToNamedEntity _, s)
}
  }
} //EML
