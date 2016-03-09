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
// EML("&")
// EML("<div>")
// EML("</div>")
// EML("""<div id="top">""")
// EML("<duff>")
// EML("&")
// EML.htmlToDisplayHTML("""<div id="top">""", true)
// EML("&#x19")

object EML {
  // ?! = negative lookahead, so fails if present
  // so fails if following text matches
  // optional(entity marks) ~ (Hexlike number | word character) ~ ;
  // i.e. looks like an HTML entity
  val ampToEntityRP = Pattern.compile("""&(?!#?[xX]?(?:[0-9a-fA-F]+|\w+);)""")
  val htmlTags = ("""(?:div|span|blockquote|pre|code|h\d|p|ul|ol|dl|li|dt|dd|img|a|i|b)""")
  val quotRP = Pattern.compile("\"")

  // "<" ~ ([lookahead] ~ optional(?) ~ tags)
  val htmlOpenTagRP = Pattern.compile("""<(?=/?""" + htmlTags + ")")
  // ([capture] tags | "\"") ~ ">"
  val htmlCloseTagRP = Pattern.compile("""(""" + htmlTags + """|")>""")
  println(s"htmlOpenTagRP: $htmlOpenTagRP")
  println(s"htmlCloseTagRP: $htmlCloseTagRP")
  println(s"ampToEntityRP: $ampToEntityRP")

  def ampToEntity(str: String)
  {

    val b = new StringBuffer()
    val m = ampToEntityRP.matcher(str)

    // Ampersand-encoding based on Nat Irons's Amputator
    // MT plugin: <http://bumppo.net/projects/amputator/>
    // ...And nicked by us from the Drupal Markdown module.
    while (m.find()) {
      m.appendReplacement(b, "&amp;")
    }
    m.appendTail(b)

    b.toString
  }

  def apply(str: String)
      : String =
  {
    // Dont do this where? In pre blocks...
    val pStart = Pattern.compile("<pre>")
    val pEnd = Pattern.compile("</pre>")
    val b = new StringBuffer()

    val m0 = quotRP.matcher(str)
    val o0 = m0.replaceAll("&quot;")
    val m1 = htmlOpenTagRP.matcher(o0)
    val o1 = m1.replaceAll("&lt;")
    val m2 = htmlCloseTagRP.matcher(o1)
    val o2 = m2.replaceAll("$1&gt;")
    val m3 = ampToEntityRP.matcher(o2)
    val o3 = m3.replaceAll("&amp;")

    o3
  }

  def htmlToDisplayHTMLNumeric(str: String)
      : String =
  {

    val b = new StringBuilder()
    str.foreach { c: Char =>
      if (c == '"' || c == '<' || c == '>' || c == '&') {
        b ++= "&#"
        b append c.toInt
        b += ';'
      }
    }
    b.toString
  }

  def htmlToDisplayHTMLNamed(str: String)
      : String =
  {

    val b = new StringBuilder()
    str.foreach { c: Char =>
      c match {
        case '"' => b ++= "&#quot;"
        case '<' => b ++= "&#lt;"
        case '>' => b ++= "&#gt;"
        case '&' => b ++= "&#amp;"
        case chr => b append chr
      }
    }
    b.toString
  }

  def htmlToDisplayHTML(str: String, asNames: Boolean)
      : String =
  {
    if (asNames) htmlToDisplayHTMLNamed(str)
    else htmlToDisplayHTMLNumeric(str)
  }

} //EML
