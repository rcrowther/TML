package tml


import java.util.regex.Matcher
import java.util.regex.Pattern




/** Utilities for html parsing.
  */
object HTMLUtils
{
  // val data = tml.FileReader("/home/rob/Code/scala/TML/text/GUIDE")
  // val r = tml.HTML.toWebpage("Guide", "/home/rob/Code/scala/TML/text/lib/doc.css",  p.result())
  /** Builds an html 5 web page from inner html.
    * 
    * Wraps the given contents in head/body/article tags and often-used
    * information.
    *
    * The page is HTML 5 and explicitly encoded as "UTF-8". It will
    * also display ASCII/Latin encodings.
    *
    * This method is intended for local testing or generation.  More
    * sophisticated needs will require more options than this method
    * provides.
    *
    * @param title
    * @param description
    * @param js
    * @param css
    * @param contents
    */
  def toWebpage(
    title: String,
    description: String,
    js: Traversable[String],
    css: Traversable[String],
    contents: String
  )
      : String =
  {
    val b = new StringBuilder()

    b ++= """<!DOCTYPE html><html><head>"""


    if (!title.isEmpty) {
      b ++= "<title>"
      b ++= title
      b ++= "</title>"
    }

    b ++= """<meta http-equiv="content-type" content="text/html; charset=UTF-8" />"""

    if (!description.isEmpty) {
      b ++= """<meta name="description" content=""""
      b ++= title
      b ++= """" />"""
    }

    if (!js.isEmpty) {
      js.foreach{ s =>
        b ++= """<script type="text/javascript" src=""""
        b ++= s
        b ++= """"></script>"""
      }
    }

    if (!css.isEmpty) {
      css.foreach{ s =>
        b ++= """<link rel="stylesheet" type="text/css" media="screen" href=""""
        b ++= s
        b ++= """"/>"""
      }
    }

    b ++= """</head><body><article>"""
    b ++= contents
    b ++= """</article></body></html>"""

    b.result()
  }

  /** Builds an html 5 web page from inner html.
    * 
    * Wraps the given contents in head/body tags and often-used
    * information.
    *
    * The page is HTML 5 and explicitly encoded as "UTF-8". It will
    * also display ASCII/Latin encodings.
    *
    * This method is intended for local testing or generation.  More
    * sophisticated needs will require more options than this method
    * provides.
    *
    * @param title
    * @param css
    * @param contents
    */
  def toWebpage(
    title: String,
    css: String,
    contents: String
  )
      : String =
  {
    toWebpage(
      title,
      "",
      Seq.empty,
      Seq(css),
      contents
    )
  }

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
      case "&" => "&amp;"
      case "<" => "&lt;"
      case ">" => "&gt;"
      //        case '"' => b ++= "&quot;"
      // source match, but no recognition match
      case _ => {
        println(s"unmatched search? $s")
        s
      }
    }
  }




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
    * As the method is for trusted data, it will not encode quotation marks. They are unlikely to appear in attribute form, and encoding is likely to clash with tpographic content.
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
  def toDisplayDefensive(str: String)
      : String =
  {
    stringReplace(defensiveP, stringToNamedEntity _, str)
  }

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
    * As the method is for trusted data, it will not encode quotation marks. They are unlikely to appear in attribute form, and encoding is likely to clash with tpographic content.
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
  def toDisplayDefensive(st: Traversable[String])
      : Traversable[String] =
  {
    st.map{ s =>
      stringReplace(defensiveP, stringToNamedEntity _, s)
    }
  }

  def toDisplay(str: String, asNames: Boolean)
      : String =
  {
    if (asNames) stringReplace(simpleP, stringToNamedEntity _, str)
    else stringReplace(simpleP, stringToNumericEntity _, str)
  }

  /** Escape html markup.
    *
    * This method is for display of HTML markings which a browser
    * would interprete as code.
    *
    * The method is provided for administrator provided/preprocessed
    * data which needs to be displayed. Do *NOT* use this method for
    * untrusted data. It will allow XSS attacks on other sites.
    *
    * The method is not guaranteed to produce intended output. It
    * encodes every ocurrence of the marks. If this is likely to clash
    * with content, use a defensive method, or map and filter the
    * content.
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
  def toDisplay(str: String)
      : String =
  {
    stringReplace(simpleP, stringToNamedEntity _, str)
  }

  /** Escape html markup.
    *
    * This method is for display of HTML markings which a browser
    * would interprete as code.
    *
    * The method is provided for administrator provided/preprocessed
    * data which needs to be displayed. Do *NOT* use this method for
    * untrusted data. It will allow XSS attacks on other sites.
    *
    * The method is not guaranteed to produce intended output. It
    * encodes every ocurrence of the marks. If this is likely to clash
    * with content, use a defensive method, or map and filter the
    * content.
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
  def toDisplay(st: Traversable[String])
      : Traversable[String] =
  {
    st.map{ s =>
      stringReplace(simpleP, stringToNamedEntity _, s)
    }
  }

}//HTMLUtils
