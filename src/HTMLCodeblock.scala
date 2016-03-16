package tml



/** Parses TML markup to generate HTML.
  *
  * This versiion of the HTML parser has a light but significant
  * addition to rendering - either `cb`, or `codeblock` as a tagname
  * generates &lt;pre> &lt;code>... &lt;/code> &lt;/pre>. So,
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
  * Note the W3 intentions: the &lt;pre> tag is block level, this
  * defines a presentation area, and `pre` should define literal
  * contents. The &lt;code> tag is character-level, representing a
  * fragment of computer code.
  *
  * If attributes are added, they will be added to the &lt;pre>
  * tag. Sematically, this allows different approaches to
  * presentation, but code is always code. This is a
  * text-based/book-like approach. If some different attribute or name
  * scheme is needed, for example, to trigger Javascript syntax
  * highlighting, copy this class and substitute the new attribute
  * name.
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
      b ++= "<pre" 
      attributesStockRender(attrs)
      b ++= "><code>"
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
    println
    println(p)
  }

  def apply()
      : HTMLCodeblock =
  {
    new HTMLCodeblock()
  }

}//HTMLCodeblock
