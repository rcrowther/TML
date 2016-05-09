package tml

import java.util.regex.Matcher
import java.util.regex.Pattern


// TODO: mapWhenNot(...)
object Util
{



  /** Parse an attribute list.
    *
    * @return tuple of [end position, tagname, klassname, seq of attributes]
    */
  // tml.Util.parseAttributes("frigging.jumper{blob of text}{blob of pyro}{blobby}", 0)
  def parseAttributes(
    line: String,
    pos: Int
  )
      : Tuple4[Int, String, String, Seq[String]] =
  {
    val t = System.nanoTime()
    var tag = ""
    var klass = ""
    var attsB = Seq.newBuilder[String]
    var fwd = 0
    var p = pos
    fwd = line.indexWhere(
      (c) => {c == ' ' ||  c == '.' || c == '{'}
    )

    if (fwd == -1) {
      tag = line.slice(p, line.size - 1)
      line.size
    }
    else {
      tag = line.slice(p, fwd)
      p = fwd
      if (line(p) == '.') {
        fwd = line.indexWhere(
          (c) => {c == ' ' || c == '{'},
          p
        )
        if(fwd != -1) {
          klass = line.slice(p + 1, fwd)
          p = fwd
        }
      }

      if (line(p) == '{') {
        var cont = true

        while (cont) {

          fwd = line.indexWhere(
            (c) => { c == '}'},
            p
          )
          // if nothing, give up
          if (fwd == -1 ) {
            attsB += line.slice(p + 1, line.size)
            p = line.size
            cont = false
          }
          else {
            attsB += line.slice(p + 1, fwd)
            p = fwd
            p += 1
            cont = ( p < line.size && line(p) == '{')
          }
        }
      }
      
    }

    println(s"time: ${System.nanoTime() - t}")
    (p, tag, klass, attsB.result)
  }



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

  /** Match a close.
    * assumes on open.
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

  /** Parse a close.
    *
    * Position is one past the control. This is ok for `slice`, even if at end of line.
*
    * @return tuple of [position, control char]. If not close, position is -1.
    */
  def parseClose(
    line: String
  )
      : Tuple2[Int, Char] =
  {
    var pos = line.indexWhere(_ != ' ')
    if (pos == -1) (-1, '\u0000')
    else {
      val c = line(pos)
      pos += 1
      if (pos >= line.size) (pos, c)
      else {
        if (Character.isWhitespace(line(pos))) (pos, c)
        else (-1, '\u0000')
      }
    }
  }

  /** Builds new strings, applying a function when not the contents of a block mark.
    *
    * The function is applied to everything outside the block mark. It
    * will not map the target marks, but other TML is mapped.
    *
    * assumes
    * @param control the mark to seek.
    * @param klass name of a class to match. If empty, will match any,
    *  or no, class.
    * @param st traversable of strings to map. Newlines must be
    *  stripped from the string ends.
    * @param f function to apply to the mark contents.
    */
  // tml.Util.mapWhenNot('?', "", tml.FileReader("/home/rob/Code/scala/TML/text/SPEC"), (s) => {print(s); "lit!"})
  // TODO: skip *all* controls --- No, ok for this...
  def mapWhenNot(
    control: Char,
    klass: String,
    st: Traversable[String],
    f: (String) => String
  )
      : Traversable[String] =
  {
    var isMapping = false


    st.map{ s =>
      var pos = s.indexWhere(_ != ' ')

      if (!isMapping) {

        // not in mapping
        // test for close
val (fwd : Int, c) = parseClose(s)
        if (fwd != -1 && c == control) {
          println(s" map on '$s'" )
          // ok, change
          isMapping = true
          // pass the mark unaltered, but map following text
          s.slice(0, fwd) + f(s.slice(fwd, s.size))
        }
        else s
      }
      else {
        // in mapping
        // test for open
        if (pos != -1 && s(pos) == control) {
          val (fwd, tag, klas, atts) = parseAttributes(
            s,
            pos
          )
          if (klas == klass) {
          println(s"  not map on '$s'" )
            // ok, change
            isMapping = false
            // pass the mark and text unaltered
            s
          }
          else {
            // map everything
            f(s)
          }
        }
        else f(s)
      }
    }
  }

  // tml.Util.mapWhen('?', "", tml.FileReader("/home/rob/Code/scala/TML/text/SPEC"), (s) => {print(s); "lit!"})
  /** Builds new strings, applying a function to the contents of a mark.
    *
    * The function is only applied to the contents of the mark, not
    * the mark itself.
    *
    * @param control the mark to seek.
    * @param klass name of a class to match. If empty, will match any, or no, class.
    * @param st traversable of strings to map. Newlines must be stripped from the string ends.
    @param f function to apply to the mark contents.
    */
  //TODO: includeInline: Boolean,
// TODO: Adapt to new attributes
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
