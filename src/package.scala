


/** Provides a crude but flexible parser for lightweight markup.
  *
  * The base is [[Parser]]. This class is unusable on it's own. 
  *
  * `Parser` has been extrended into [[HTML]], for the generation of
  * HTML.
  *
  * {{{
  * val p = new tml.HTML()
  * p.parse(<string with TML markup>)
  * p.result()
  * p.clear()
  * }}}
  *
  *
  * ==UML==
  * [[UML]] is a lightweight language for character markup,
  * converting characters into Unicode. It handles conversion to
  * Unicode copyright marks, quotes, fractions, accents, and others.
  * {{{
  * val res = UML(<string with UML markup>)
  * }}}
  *
  * Will return simple text marked up with Unicode entities.
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
