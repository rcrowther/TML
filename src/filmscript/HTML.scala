package tml
package filmscript


/** Parses TML-like markup to generate screenscript HTML.
  *
  */
// tml.filmscript.HTML.errorReport(tml.InputIterator(tml.FileReader("""/home/rob/Code/scala/TML/test/filmscript/test.tml""")))
class HTML(val ot: OutputTarget)
    extends Parser
    with filmscript.Definitions
{
  //\n<p class=\"transition\">FADE OUT.</p></section>
  val close: String = "<p class=\"transition close\">THE END</p></article>"
  val open: String = "<article class=\"filmscript-en\">\n"

  // nightmare---place text at complex mid-position
  // means the parser can not be reused.
  var firstSection = true


  /** Test on construction that all marks are different.
    */
  verifyControlDefinitions()

  // Renderers

  // div, main, section, article, aside
  // ol, ul, dl,
  // li, dd, dt, blockquote
  def renderBlockOpen(md: MarkData)
  {
    md.control match {
      // Title
      case '?' => {
        //
        ot ++= "<header class=\"title\">\n<h1>"
        ot ++= md.params(0)
        ot ++= "</h1>"
      }

      // scene
      case '=' => {
        ot ++= "\n<section>\n<header>"
        if (firstSection) {
          ot ++= "<p class=\"transition open\">FADE IN:</p>"
          firstSection = false
        }
        ot ++= "<h5>"
        ot ++= md.params(0)
        ot ++= "</h5></header>"
      }

      // character
      case '#' =>  {
        ot ++= "\n<div class=\"action\">\n<h6>"
        ot ++= md.params(0)
        ot ++= "</h6>"
      }

      // shotseries
      case '+' =>
        {
          ot ++= "\n<p class=\"instruction\">"
          ot ++= md.params(0)
          ot ++= "</p>\n<ol>"
        }

        // shot
        /*
         case '-' =>
         {
         ot ++= """<p class="instruction">"""
         ot ++= md.params(0)
         ot ++= "</p><ol>"
         }
         */

      // simultaneous dialogue
      case '|' =>  {
        ot ++= "\n<div class=\"simultaneous\">\n"
      }

      // on-screen text
      case '>' =>  {
        ot ++= "\n<div class=\"action on-screen-text\">\n"
      }

      case x => println(s"unknown control '$x'")
    }

  }

  def renderBlockClose(
    md: MarkData
  )
  {
    //println(s"renderBlockClose $md")
    md.control match {
      // Title
      case '?' => ot ++= "</header>"

      // scene
      case '=' => ot ++= "</section>"

      // character
      case '#' =>  ot ++= "</div>"

      // shotseries
      case '+' => ot ++= "</ol>"

      // simultaneous dialogue
      case '|' =>  {
        ot ++= "\n</div>"
      }

      // on-screen text
      case '>' =>  {
        ot ++= "\n</div>"
      }
      // shot
      /*
       case '-' =>
       {
       ot ++= """<p class="instruction">"""
       ot ++= md.params(0)
       ot ++= "</p><ol>"
       }
       */
      case x => println(s"unknown control '$x'")
    }
  }

  // used for hr
  def renderBlockSelfClosingMark(md: MarkData)
  {
    ot += '<'
    ot ++= md.resolvedTagname
    //classAttributeRender(md)
    ot ++= "/>"

    //logger.attributeRangeWarning(md, 0)
  }

  // used for h?, pre
  def renderParagraphOpen(md: MarkData)
  {
    // fix the headline tag name
    // headlines are a little complex.
    // If there is no prefixed control char in the name,
    // then the tag name was given or defaulted, so do nothing.
    // If prefixes exist, they are counted to form the
    // resolvedTagname.
    /*
     if (BlockParagraphNoDefaultMarks.contains(md.control)) {
     md.resolvedTagname = "h" + (md.tagName.length + 1)
     }

     ot += '<'
     ot ++= md.resolvedTagname
     //classAttributeRender(md)
     ot += '>'
     */

    md.control match {
      // transition
      case '~' => ot ++= "\n<p class=\"transition\">"
      // instruction
      case '*' => ot ++= "\n<p class=\"instruction\">"
      //shot
      case '@' => ot ++= "<li>"
      case _ => //TODO:
    }
    //logger.attributeRangeWarning(md, 0)
  }

  def renderParagraphClose(md: MarkData)
  {
    md.control match {

      // transition
      case '~' => ot ++= "</p>"
      // instruction
      case '*' => ot ++= "</p>"
      //shot
      case '@' => ot ++= "</li>"
      case _ =>
    }
  }


  def renderTextParagraphOpen()
  {
    ot ++= "\n<p>"
  }

  def renderTextParagraphClose()
  {
    ot ++= "</p>"
  }


  // used for a, i, b, span
  def renderInlineOpen(md: MarkData)
  {

  }


  def renderInlineClose(
    md: MarkData
  )
  {

  }




  // Used for img
  def renderInlineSelfClosingMark(md: MarkData)
  {

  }

}//HTML



object HTML
    extends ParserCompanion[HTML]
{

  // val i = tml.InputIterator("/home/rob/Code/scala/TML/text/SPEC")
  // tml.FileReader("""/home/rob/Code/scala/TML/text/SPEC""")
  // tml.HTML(tml.FileReader.stream("""/home/rob/Code/scala/TML/text/SPEC"""))


  def builder(ot: OutputTarget)
      : HTML =
  {
    new HTML(ot)
  }

}//HTML
