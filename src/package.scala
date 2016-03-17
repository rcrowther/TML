


/** Provides a crude but flexible parser for lightweight markup.
  *
  * The base is [[Parser]]. This class is unusable on it's own. 
  *
  * ==TML==
  * `Parser` has been extended into [[HTML]], for the generation of
  * HTML. Use the quick test `apply` method,
  *
  * {{{
  * tml.HTML("=== Some charms\n[a{coffee.fr} Not this] for now. So what?")
  * }}}
  *
  * Store a reusable parser,
  *
  * {{{
  * val p = tml.HTML()
  * p(<string with TML markup>)
  * p.result()
  * p.clear()
  * }}}
  *
  *
  * ===Input===
  * Input to the parser is by a wrapping class [[InputIterator]].
  * 
  * InputIterator wraps several fundamentally different options,
  * including a string, multiple strings while
  * restoring newlines (Java code often strips these on a fileread) or
  * an [[java.io.InputStreamReader]].
  *
  * Implicits are provided, so there is often no need to state the
  * wrap explicitly. This may only be required if a special
  * InputIterator is required, for example, the iterator which
  * restores lineends,
  *
  * {{{
  * val p = tml.HTML()
  * p(InputIterator.line(<collection of strings with line ends stripped>))
  * p.result()
  * p.clear()
  * }}}
  *
  * ===Parser Extensions===
  * Extended from the basic parser are,
  *
  *  - [[HTML]] for basic HTML
  *  - [[HTMLCodeblock]] HTML plus a special tagname 'codeblock'/'cb', which writes a pre/code tag.
  *
  * 
  * ===*Utils===
  * Each parser may have a corresponding `Utils` object, for example,
  * [[HTMLUtils]].  HTMLUtils has methods for escaping and webpage
  * building.
  *
  * 
  * ==File reading==
  * The mini-filereader is built-in to preserve your sanity (no, no, I
  * mean, "for ease of assessment and testing"). Returning a stream,
  * and assuming implicit wrap to an InputIterator,
  *
  * {{{
  * tml.HTML(tml.FileReader("""/home/<some filepath>/TML/text/SPEC"""))
  * }}}
  *
  * [[tml.FileReader]] is overloaded to accept paths of type `String`,
  * `File`, or `Path`.
  *
  * 
  * ==UML==
  * [[UML]] is a lightweight language for character markup, converting
  * characters into Unicode. Markup codes exist for copyright marks,
  * quotes, fractions, accents, and others.
  * 
  * {{{
  * val res = tml.UML(<string with UML markup>)
  * }}}
  *
  * Will return simple text marked up with Unicode entities. The
  * package documentation contains full details of the markup
  * available.
  *
  * 
  * ==Alternatives==
  * Many. See
  * [[https://en.wikipedia.org/wiki/Lightweight_markup_language]].
  *
  * e.g.
  *
  * [[http://projectmallard.org/about/learn/code]]
  *
  * [[https://en.wikipedia.org/wiki/ReStructuredText]]
  *
  * [[http://daringfireball.net/projects/markdown/]]
  *
  * @define pkg tml
  */
package object tml {

}//package
