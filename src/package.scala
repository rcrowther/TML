


/** Provides a crude but flexible parser for lightweight markup.
  *
  * The base is [[Parser]]. This class is unusable on it's own. 
  *
  * `Parser` has been extended into [[HTML]], for the generation of
  * HTML. Use the quick test `apply` method,
  *
  * {{{
  * tml.HTML("=== Some charms\n[a{coffee.fr} Not this] for now. So what?")
  * }}}
  *
  * Store a reusable parser with the full methodology,
  *
  * {{{
  * val p = tml.HTML()
  * p(<string with TML markup>)
  * p.result()
  * p.clear()
  * }}}
  *
  *
  * Use the mini-filereader, built-in to preserve your sanity (no,
  * no, I mean, "for ease of assessment and testing"),
  *
  * {{{
  * tml.HTML(tml.FileReader("""/home/<some filepath>/TML/text/SPEC"""))
  * }}}
  *
  * [[tml.FileReader]] is overloaded to accept paths of type `String`,
  * `File`, or `Path`.
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
  * ==EML==
  * [[EML]] is an object carrying methods for HTML escaping.
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
