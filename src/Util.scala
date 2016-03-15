package tml

import java.util.regex.Matcher
import java.util.regex.Pattern


// TODO: mapWhenNot(...)
object Util
{


  private def findOpen(
    line: String,
    control: Char,
    klass: String
  )
      : Int =
  {


    var pos = line.indexWhere(_ != ' ')

    if (pos == -1 || line(pos) != control) -1
    else {
      var mark = 1
      var cont = true

      while (cont) {

        mark match {
          case 1 => {
            // skip tagname (no spaces allowed)
            pos = line.indexWhere(
              (c) => {c == '.' || c == '[' || c == '{' || c == ' ' },
              pos
            )
            // if nothing, entire line must be tagname
            if (pos == -1) { cont = false; pos = line.size }
            else {
              val nextMark = line(pos)
              // get class (no spaces allowed)
              line(pos) match {
                // space, must be end
                case ' ' =>  { pos += 1; cont = false }
                case '.' => mark = 1
                case '[' => mark = 2
                case '{' => mark = 3
              }
            }
          }
          case 2 => {
            // skip class (no spaces allowed)
            val start = pos
            pos = line.indexWhere(
              (c) => { c == '[' || c == '{' || c == ' ' },
              pos
            )
            // if nothing, all following must be class
            if (pos == -1) {
              if(line.substring(start, line.size) != klass) -1
              else {
                cont = false; pos = line.size
              }
            }
            else {
              // get class (no spaces allowed)
              line(pos) match {
                // space, must be end
                case ' ' =>  {
                  if(line.substring(start, pos - 1) != klass) -1
                  else {
                    cont = false; pos += 1
                  }
                }
                case '[' => mark = 3
                case '{' => mark = 4
              }
            }
          }

          case 3 => {
            // skip text
            pos = line.indexWhere(
              (c) => {c == ']'},
              pos
            )
            // if nothing, all following must be attribute
            if (pos == -1) { cont = false; pos = line.size }
            else {
              // if not, check after
              pos += 1
              if (pos >= line.size) cont = false
              else {
                if(line(pos) != '{') {
                  // put after any (no check, should be space)
                  pos += 1
                  cont = false
                }
                else { mark = 4 }
              }
            }
          }

          case 4 => {
            // skip url
            pos = line.indexWhere(
              (c) => {c == '}'},
              pos
            )
            // if nothing, all following must be attribute
            if (pos == -1) { cont = false; pos = line.size }
            else {
              // put after any (no check, should be space)
              pos += 2
              cont = false
            }
          }
        }//match
        
      }//while

      pos
    }
  }

  /** assumes on open.
    */


 private def findClose(
    line: String,
    control: Char
  )
      : Int =
  {
    var pos = line.indexWhere(_ != ' ')

    if (pos == -1 || line(pos) != control) -1
    else {
      pos += 1
      if (pos >= line.size) pos
      else {
        if (Character.isWhitespace(line(pos))) pos + 1
        else -1
      }
    }
  }



  // tml.Util.mapWhen('?', "", tml.FileReader("/home/rob/Code/scala/TML/text/SPEC"), (s) => {print(s); "lit!"})
  /** Builds new strings, applying a function to the contents of a mark.
    *
    * The function is only applied to the contents of the mark, not the mark itself.
    *
    * @param control the mark to seek.
    * @param klass name of a class to match. If empty, will match any, or no, class.
    * @param st traversable of strings to map. Newlines must be stripped from the string ends.
    @param f function to apply to the mark contents.
    */
  //TODO: includeInline: Boolean,
// TODO: skip *all* controls --- No, ok for this...
  def mapWhen(
    control: Char,
    klass: String,
    st: Traversable[String],
    f: (String) => String
  )
      : Traversable[String] =
  {
    var isMapping = false


    st.map{ s =>
      if (!isMapping) {
        val openPoint = findOpen(
          s,
          control,
          klass
        )
        println(s"openPoint $openPoint size ${st.size}" )
        if (openPoint == -1) s
        else {
          //println(s"  map on '$s'" )
          isMapping = true
          // pass the mark unaltered, but map following text
          // NB: substring(from, until)
          s.substring(0, openPoint) + f(s.substring(openPoint, s.size))
        }
      }
      else {
        // in mapping
        val closePoint = findClose(
          s,
          control
        )
        if (closePoint == -1) f(s)
        else {
          //println(s"  closePoint" )
          // is closepoint
          isMapping = false
          // pass mark and following text unaltered
          s
        }
      }
    }
  }

}//TML
