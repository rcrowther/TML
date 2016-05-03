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


    def cacheShiftLeft(off: Int)
    {
      off match {
        case 0 => {}

        case 1 => {
          cache(0) = cache(1)
          cache(1) = cache(2)
          cache(2) = it.next()
        }
        case 2 => {
          cache(0) = cache(2)
          cache(1) = it.next()
          cache(2) = it.next()
        }

        case _ => throw new Exception(s"Offset out of range:$off")
      }
    }


    def cacheReplace3(newChar: Char)
        : Boolean =
    {
      cache(0) = newChar
      cache(1) = it.next()
      cache(2) = it.next()
      true
    }

    def cacheReplace1(newChar: Char)
        : Boolean =
    {
      cache(0) = newChar
      true
    }

    def cacheReplace2(newChar: Char)
        : Boolean =
    {
      cache(0) = newChar
      cache(1) = cache(2)
      cache(2) = it.next()
      true
    }

    def cacheInsert2For3(newChar: Char)
        : Boolean =
    {
      cache(0) = cache(2)
      cache(1) = newChar
      cache(2) = it.next()
      true
    }


    /**
      @return cache read limit
      */
    def cacheToUML()
        : Boolean =
    {
      var point1 = cache(0)
      var point2 = cache(1)
      var point3 = cache(2)

      // elipsis
      point1 match {
        case '.' => {
          if (point2 == '.' && point3 == '.') cacheReplace3('\u2026')
          else false
        }

        case '(' => {
          if (
            (point2 == 'c' || point2 == 't' || point2 == 'r')
              && point3 == ')'
          )
          {
            point2 match {
              case 'c' => cacheReplace3('\u00A9')
              case 't' => cacheReplace3('\u2122')
              case 'r' => cacheReplace3('\u00AE')
            }
          }
          else false
        }

        case '-' => {
          // em
          if (point2 == '-' && point3 == '-') {
            cacheReplace3('\u2014')
          }
          else {
            // en
            if (point2 == '-') {
              cacheReplace2('\u2013')
            }
            else {
              // dict/hyphen
              if (point2 == '.') {
                cacheReplace2('\u2027')
              }
              else cacheReplace1('\u2010')
            }
          }
        }
        case  '\'' => {
          if (point2 == '\'') cacheReplace2('\u2018')
          else cacheReplace1('\u2019')
        }
        case  '"' => {
          // double quotes
          if (point2 == '"') cacheReplace2('\u201C')
          else cacheReplace1('\u201D')
        }
        case  '<' => {
          // guillemet open
          if (point2 == '<') cacheReplace2('\u00AB')
          else false
        }
        case  '>' => {
          // guillemet close
          if (point2 == '>') cacheReplace2('\u00BB')
          else false
        }
        case ':' => {
          if (point2 == 'm') {
            // Math
            point3 match {
              // multiply, or dimensions
              case 'x' =>  cacheReplace3('\u00D7')
              // minus
              case '-' =>  cacheReplace3('\u2212')
              // degree
              case 'o' =>  cacheReplace3('\u00B0')
              case _ => false
            }
          }
          else {
            //fractions
            if (point2 > '0' && point2 < '8') {
              point2 match {
                case '1' => {
                  point3 match {
                    case '2' => cacheReplace3('\u00BD')
                    case '3' => cacheReplace3('\u2153')
                    case '4' => cacheReplace3('\u00BC')
                    case '5' => cacheReplace3('\u2155')
                    case '6' => cacheReplace3('\u2159')
                    case '8' => cacheReplace3('\u215B')
                    case _ => false
                  }
                }

                case '2' => {
                  point3 match {
                    case '3' => cacheReplace3('\u2154')
                    case '5' => cacheReplace3('\u2156')
                    case _ => false
                  }
                }

                case '3' => {
                  point3 match {
                    case '4' => cacheReplace3('\u00BE')
                    case '5' => cacheReplace3('\u2157')
                    case '8' => cacheReplace3('\u215C')
                    case _ => false
                  }
                }

                case '4' => {
                  point3 match {
                    case '5' => cacheReplace3('\u2158')
                    case _ => false
                  }
                }

                case '5' => {
                  point3 match {
                    case '6' => cacheReplace3('\u215A')
                    case '8' => cacheReplace3('\u215D')
                    case _ => false
                  }
                }

                case '7' => {
                  point3 match {
                    case '8' => cacheReplace3('\u215E')
                    case _ => false
                  }
                }
                case _ => false
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
                      case 'A' => cacheReplace3('\u00C0')
                      case 'E' => cacheReplace3('\u00C8')
                      case 'O' => cacheReplace3('\u00D2')
                      case 'I' => cacheReplace3('\u00CC')
                      case 'U' => cacheReplace3('\u00D9')
                      case 'a' => cacheReplace3('\u00E0')
                      case 'e' => cacheReplace3('\u00E8')
                      case 'i' => cacheReplace3('\u00EC')
                      case 'o' => cacheReplace3('\u00F2')
                      case 'u' =>  cacheReplace3('\u00F9')
                      case _ => cacheInsert2For3('\u0300')
                    }
                  }

		  case '/' => {
                    point3 match {
                      case 'A' => cacheReplace3('\u00C1')
                      case 'E' => cacheReplace3('\u00C9')
                      case 'I' => cacheReplace3('\u00CD')
                      case 'O' => cacheReplace3('\u00D3')
                      case 'U' => cacheReplace3('\u00DA')
                      case 'a' => cacheReplace3('\u00E1')
                      case 'e' => cacheReplace3('\u00E9')
                      case 'i' => cacheReplace3('\u00ED')
                      case 'o' => cacheReplace3('\u00F3')
                      case 'u' => cacheReplace3('\u00FA')
                      case 'y' => cacheReplace3('\u00FD')
                      case _ => cacheInsert2For3('\u0301')
                    }
                  }

		  case '^' => {
                    point3 match {
                      case 'A' => cacheReplace3('\u00C2')
                      case 'E' => cacheReplace3('\u00CA')
                      case 'I' => cacheReplace3('\u00CE')
                      case 'O' => cacheReplace3('\u00D4')
                      case 'U' => cacheReplace3('\u00DB')
                      case 'a' => cacheReplace3('\u00E2')
                      case 'e' => cacheReplace3('\u00EA')
                      case 'i' => cacheReplace3('\u00EE')
                      case 'o' => cacheReplace3('\u00F4')
                      case 'u' => cacheReplace3('\u00FB')
                      case _ => cacheInsert2For3('\u0302')
                    }
                  }

		  case 'u' => {
                    cacheInsert2For3('\u0306')
                  }

		  case ':' => {
                    point3 match {
                      case 'A' => cacheReplace3('\u00C4')
                      case 'E' => cacheReplace3('\u00CB')
                      case 'I' => cacheReplace3('\u00CF')
                      case 'O' => cacheReplace3('\u00D6')
                      case 'U' => cacheReplace3('\u00DC')
                      case 'a' => cacheReplace3('\u00E4')
                      case 'e' => cacheReplace3('\u00EB')
                      case 'i' => cacheReplace3('\u00EF')
                      case 'o' => cacheReplace3('\u00F6')
                      case 'u' => cacheReplace3('\u00FC')
                      case 'y' => cacheReplace3('\u00FF')
                      case _ => cacheInsert2For3('\u0308')
                    }
                  }

		  case 'o' => {
                    point3 match {
                      case 'A' => cacheReplace3('\u00C5')
                      case 'a' => cacheReplace3('\u00E5')
                      case _ => cacheInsert2For3('\u030A')
                    }
                  }
		  case 'v' => {
                    cacheInsert2For3('\u030C')
                  }
		  case 'c' => {
                    point3 match {
                      case 'c' => cacheReplace3('\u00E7')
                      case 'C' => cacheReplace3('\u00C7')
                      case _ => cacheInsert2For3('\u0327')
                    }
                  }

                  case _ => false
                }
              }
              else false
            }
          }
        }

        case _ => false
      }

      //cache(0) = point1
      //cache(1) = point2
      //cache(2) = point3
      //readLimit

    }


    // Stores points tested for UML, maybe substituted
    private var cache = new Array[Char](3)

    // Current cache item for next()
    // -1 = cache not in action, or an index to cache
    private var cacheI = -1
    private val limit = 3

    def next() : Char =
    {
      val ret =
        if (cacheI < 0) {
          val rp = it.next()

          // test if point is maybe UML, if so, cache
          val r =
            if (isUMLStart(rp)) {
              // point is possible UML
              // load cache, process, set cacheI
              cacheReplace3(rp)
              cacheToUML()
              cacheI = 1
              cache(0)
            }
            else rp
          r
        }
        else {
          // empty the cache

          // test if the point is maybe UML, if so,
          // cache shift left, process, set cacheI
          val rc =
            if (isUMLStart(cache(cacheI))) {
              cacheShiftLeft(cacheI)
              cacheToUML()
              cacheI = 1
              cache(0)
            }
            else {
              // not UML. Advance.
              val r = cache(cacheI)
              cacheI += 1
              if (cacheI >= limit) cacheI = -1
              r
            }
          rc
        }
      ret
    }

    def lookForward : Char = it.lookForward

  }

}//UMLInputIterator
