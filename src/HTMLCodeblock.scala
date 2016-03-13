package tml



/** Parses TML markup to generate HTML.
  *
  * This versiion of the HTML parser has a light but significant
  * addition to rendering - either `cb`, or `codeblock` as a tagname
  * generates &lt;pre> &ltcode>; &lt;/code> &lt;/pre>. So,
  *
  * {{{
  * ?codeblock
  *  <some computer code>
  * ?
  * }}}
  * 
  * Will both render the contents literally, and wrap in the W3 recommended
  * HTML tags. 
  *
  * If a class attribute is added, this will also work with some
  * Javascript syntax highlighters.
  */
class HTMLCodeblock
    extends HTML
{

  override val blockBracketedTagnameAliases = Map(
    "d" -> "div",
    "c" -> "code",
    "bq" -> "blockquote",
    "cb" -> "codeblock"
  )


  // div, main, section, article, aside
  // ol, ul, dl,
  // li, dd, dt, blockquote
  override def renderBlockOpen(attrs: MarkAttributes) = {
    if(attrs.resolvedTagname == "codeblock") {
      b ++= "<pre><code"
      attributesStockRender(attrs)
      b += '>'
    }
    else {
      super.renderBlockOpen(attrs)
    }
  }

  override def renderBlockClose(
    attrs: MarkAttributes
  )
  {
    if(attrs.resolvedTagname == "codeblock") {
      b ++= "</code></pre>"
    }
    else {
      super.renderBlockClose(attrs)
    }
  }


}//HTMLCodeblock



object HTMLCodeblock {


  // tml.FileReader("""/home/rob/Code/scala/TML/text/SPEC""")
  // tml.HTMLCodeblock(tml.FileReader("""/home/rob/Code/scala/TML/text/SPEC"""))

  def apply(it: InputIterator)
  {
    println("running apply")
    val p = new HTMLCodeblock()
    p(it)
    p.blockBalance(fix = false)
    println
    println(p.errorLog.toText())
    println("out:")
    println(s"'${p.result()}'")
    println(p)
  }

  def apply()
      : HTMLCodeblock =
  {
    new HTMLCodeblock()
  }

}//HTMLCodeblock
