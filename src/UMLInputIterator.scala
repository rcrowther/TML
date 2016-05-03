package tml


import java.util.regex.Matcher
import java.util.regex.Pattern

import scala.language.implicitConversions

//val i = tml.UMLInputIterator(tml.InputIterator(tml.FileReader("/home/rob/Code/scala/TML/test/UMLShort")))

/** Markup for creating inaccessible unicode codepoints.
  *
  * 30 years on we still do not have keyboards or text processors
  * which handle extended character sets in any easy manner. Most
  * users are unaware of the existence of these codes, and unaware of
  * how to write them into a document.
  *
  * This set of methods is not designed to be the answer to the
  * problem. It is an easy-to-understand way of creating common
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
  * example, the code may damage pre-formed HTML pages where
  * &lt;script> or &lt;head> tags exist.
  *
  * Java strings are encoded as UTF-16, and string handling mostly
  * works in UTF-16. Thus this Scala implementation of UML does not
  * use entities, but punches in the necessary codepoints. Please bear
  * in mind that the results of a parse, even if correct, may display,
  * in display code not handling UTF-16, as distorted.
  *
  * [[http://alistapart.com/article/emen]]
  *
  * ==Alternatives==
  * Smartypants
  * [[http://daringfireball.net/projects/smartypants/]]
  */
//TODO: soft hyphen (&#173;
object UMLInputIterator {




  /** Creates a UML transforming iterator from another input iterator.
    *
    * Codings may be specified on the supplied InputIterator.
    *
    * For guides to UML markup, see the string modification methods,
    * or look at the UML Spec.
    */
  // This iterator is if..then parsed. This makes it difficult to
  // read and maintain, but the result should be fast (it will
  // probably outpace regex by a reasonable margin)
  def apply(it: InputIterator): InputIterator = new InputIterator {

    def isUMLStart(c: Char)
        : Boolean =
    {
      (
        c == '.'
          || c == '('
          || c == '-'
          || c == '\''
          || c == '"'
          || c == '<'
          || c == '>'
          || c == ':'
      )
    }

    /**
      @return cache read limit
      */
    def cacheToUML()
        : Int =
    {
      // lookahead saw the first point as a possible UML
      var point1 = it.next()
      var point2 = it.next()
      var point3 = it.next()

      val readLimit =
        // elipsis
        point1 match {
          case '.' => {
            if (point2 == '.' && point3 == '.') {point1 = '\u2026'; 1}
            else 3
          }

          case '(' => {
            if (
              (point2 == 'c' || point2 == 't' || point2 == 'r')
                && point3 == ')'
            )
            {
              point2 match {
                case 'c' => {point1 = '\u00A9'; 1}
                case 't' => {point1 = '\u2122'; 1}
                case 'r' => {point1 = '\u00AE'; 1}
              }
            }
            else 3
          }

          case '-' => {
            // em
            if (point2 == '-' && point3 == '-') {
              {point1 = '\u2014'; 1}
            }
            else {
              // en
              if (point2 == '-') {
                {point1 = '\u2013'; 1}
              }
              else {
                // em/dict
                if (point2 == '.') {
                  {point1 = '\u2027'; 1}
                }
                else {point1 = '\u2010'; 1}
              }
            }
          }
          case  '\'' => {
            if (point2 == '\'') {point1 = '\u2018'; 1}
            else {point1 = '\u2019'; 1}
          }
          case  '"' => {
            // double quotes
            if (point2 == '"') {point1 = '\u201C'; 1}
            else {point1 = '\u201D'; 1}
          }
          case  '<' => {
            // guillemet open
            if (point2 == '<') {point1 = '\u00AB'; 1}
            else 3
          }
          case  '>' => {
            // guillemet close
            if (point2 == '>') {point1 = '\u00BB'; 1}
            else 3
          }
          case ':' => {
            if (point2 == 'm') {
              // Math
              point3 match {
                // multiply, or dimensions
                case 'x' =>  {point1 = '\u00D7'; 1}
                // minus
                case '-' =>  {point1 = '\u2212'; 1}
                // degree
                case 'o' =>  {point1 = '\u00B0'; 1}
                case _ => 3
              }
            }
            else {
              //fractions
              if (point2 > '0' && point2 < '8') {
                point2 match {
                  case '1' => {
                    point3 match {
                      case '2' => {point1 = '\u00BD'; 1}
                      case '3' => {point1 = '\u2153'; 1}
                      case '4' => {point1 = '\u00BC'; 1}
                      case '5' => {point1 = '\u2155'; 1}
                      case '6' => {point1 = '\u2159'; 1}
                      case '8' => {point1 = '\u215B'; 1}
                      case _ => 3
                    }
                  }

                  case '2' => {
                    point3 match {
                      case '3' => {point1 = '\u2154'; 1}
                      case '5' => {point1 = '\u2156'; 1}
                      case _ => 3
                    }
                  }

                  case '3' => {
                    point3 match {
                      case '4' => {point1 = '\u00BE'; 1}
                      case '5' => {point1 = '\u2157'; 1}
                      case '8' => {point1 = '\u215C'; 1}
                      case _ => 3
                    }
                  }

                  case '4' => {
                    point3 match {
                      case '5' => {point1 = '\u2158'; 1}
                      case _ => 3
                    }
                  }

                  case '5' => {
                    point3 match {
                      case '6' => {point1 = '\u215A'; 1}
                      case '8' => {point1 = '\u215D'; 1}
                      case _ => 3
                    }
                  }

                  case '7' => {
                    point3 match {
                      case '8' => {point1 = '\u215E'; 1}
                      case _ => 3
                    }
                  }
                  case _ => 3
                }
              }
              else {
                // accents
                if (
                  point2 == '\\'
                    || point2 == '/'
                    || point2 == '^'
                    || point2 == 'u'
                    || point2 == ':'
                    || point2 == 'o'
                    || point2 == 'v'
                    || point2 == 'c'
                )
                {
                  point2 match {
		    case '\\' => {
                      point3 match {
                        case 'A' => {point1 = '\u00C0'; 1}
                        case 'E' => {point1 = '\u00C8'; 1}
                        case 'O' => {point1 = '\u00D2'; 1}
                        case 'I' => {point1 = '\u00CC'; 1}
                        case 'U' => {point1 = '\u00D9'; 1}
                        case 'a' => {point1 = '\u00E0'; 1}
                        case 'e' => {point1 = '\u00E8'; 1}
                        case 'i' => {point1 = '\u00EC'; 1}
                        case 'o' => {point1 = '\u00F2'; 1}
                        case 'u' =>  {point1 = '\u00F9'; 1}
                        case _ => {point1 = point3; point2 = '\u0300'; 2}
                      }
                    }

		    case '/' => {
                      point3 match {
                        case 'A' => {point1 = '\u00C1'; 1}
                        case 'E' => {point1 = '\u00C9'; 1}
                        case 'I' => {point1 = '\u00CD'; 1}
                        case 'O' => {point1 = '\u00D3'; 1}
                        case 'U' => {point1 = '\u00DA'; 1}
                        case 'a' => {point1 = '\u00E1'; 1}
                        case 'e' => {point1 = '\u00E9'; 1}
                        case 'i' => {point1 = '\u00ED'; 1}
                        case 'o' => {point1 = '\u00F3'; 1}
                        case 'u' => {point1 = '\u00FA'; 1}
                        case 'y' => {point1 = '\u00FD'; 1}
                        case _ => {point1 = point3; point2 = '\u0301'; 2}
                      }
                    }

		    case '^' => {
                      point3 match {
                        case 'A' => {point1 = '\u00C2'; 1}
                        case 'E' => {point1 = '\u00CA'; 1}
                        case 'I' => {point1 = '\u00CE'; 1}
                        case 'O' => {point1 = '\u00D4'; 1}
                        case 'U' => {point1 = '\u00DB'; 1}
                        case 'a' => {point1 = '\u00E2'; 1}
                        case 'e' => {point1 = '\u00EA'; 1}
                        case 'i' => {point1 = '\u00EE'; 1}
                        case 'o' => {point1 = '\u00F4'; 1}
                        case 'u' => {point1 = '\u00FB'; 1}
                        case _ => {point1 = point3; point2 = '\u0302'; 2}
                      }
                    }

		    case 'u' => {
                      point1 = point3
                      point2 = '\u0306'
                      2
                    }

		    case ':' => {
                      point3 match {
                        case 'A' => {point1 = '\u00C4'; 1}
                        case 'E' => {point1 = '\u00CB'; 1}
                        case 'I' => {point1 = '\u00CF'; 1}
                        case 'O' => {point1 = '\u00D6'; 1}
                        case 'U' => {point1 = '\u00DC'; 1}
                        case 'a' => {point1 = '\u00E4'; 1}
                        case 'e' => {point1 = '\u00EB'; 1}
                        case 'i' => {point1 = '\u00EF'; 1}
                        case 'o' => {point1 = '\u00F6'; 1}
                        case 'u' => {point1 = '\u00FC'; 1}
                        case 'y' => {point1 = '\u00FF'; 1}
                        case _ => {point1 = point3; point2 = '\u0308'; 2}
                      }
                    }

		    case 'o' => {
                      point3 match {
                        case 'A' => {point1 = '\u00C5'; 1}
                        case 'a' => {point1 = '\u00E5'; 1}
                        case _ => {point1 = point3; point2 = '\u030A'; 2}
                      }
                    }
		    case 'v' => {
                      point1 = point3
                      point2 = '\u030C'
                      2
                    }
		    case 'c' => {
                      point3 match {
                        case 'c' => {point1 = '\u00E7'; 1}
                        case 'C' => {point1 = '\u00C7'; 1}
                        case _ => {point1 = point3; point2 = '\u0327'; 2}
                      }
                    }

                    case _ => 3
                  }
                }
                else 3
              }
            }
          }

          case _ => 3
        }

      cache(0) = point1
      cache(1) = point2
      cache(2) = point3
      readLimit
    }


    // Stores points tested for UML, maybe substituted
    private var cache = new Array[Int](3)

    // Current cache item for next()
    // -1 = cache not in action, or an index to cache
    private var cacheI = -1
    private var limit = 0

    def next() : Char =
    {
      val ret =
        if (cacheI < 0) {
          val r = it.next()

          // test if nextchar is maybe UML, if so,
          // walk ahead and cache
          if (isUMLStart(it.lookForward)) {
            cacheI = 0
            limit = cacheToUML()
          }
          r
        }
        else {
          // empty the cache
          val r = cache(cacheI)
          cacheI += 1
          if (cacheI == limit) cacheI = -1
          r
        }

      ret.toChar
    }

    def lookForward : Char = it.lookForward

  }

}//UMLInputIterator
