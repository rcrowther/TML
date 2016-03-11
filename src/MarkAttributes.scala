package tml



/** Carries data used for rendering a mark.
  *
  * Carries the original control also, so it can resolve the tag name.
  *
  * @param control the char used to identify a mark.
  * @param tagName attribute representing the name/type of the mark.
  * @param klass attribute representing the class of the mark.
  * @param url attribute representing a url attached to the mark.
  * @param text attribute representing text attached to the mark.
  */
class MarkAttributes(
  var control: Char,
  var tagName: String,
  var klass: String,
  var url: String,
  var text: String
)
{

  /** The final name/type of a mark.
    *
    * The mark may have the tag stated explicitly.  If a mark has a
    * repeated character, then the name is derived from the defaults
    * listed in the parser (depending on the control character).
    *
    * However the tag name is decided, this field carries the result.
    */
  var resolvedTagname: String = ""


  /** Sets the resolved name from a map of controls to defaults.
    *
    * If the tagname is only one char long, and matches the control,
    * then the default is used, otherwise the tag name is used.
    *
    * @param default tagname to use if the mark is defaulted.
    */
  def tagNameResolve(default: String)
  {
    // NB: The zero test is wasted on block opening marks
    // but no big deal.
    resolvedTagname =
      if (tagName.size == 0 || (tagName.size == 1 && tagName(0) == control)) {
        default
      }
      else tagName
  }


  /** Sets the resolved name from a map of controls to defaults.
    *
    * If the tagname is only one char long, and matches the control,
    * then the default is used, otherwise the tag name is used.
    *
    * @param map a map of chars to defaults.
    */
  def tagNameResolve(map: Map[Char, String])
  {
    // NB: The zero test is wasted on block opening marks
    // but no big deal.
    resolvedTagname =
      if (tagName.size == 0 || (tagName.size == 1 && tagName(0) == control)) {
        map(control)
      }
      else tagName
  }

  /** Number of times the control appears at the start of the tag.
    *
    * Used to count multiple control repetitions e.g. for headline
    * controls.
    */
  def tagControlPrefixCount
      : Int =
  {
    var i = 0
    var cont = true

    while ((i < tagName.size) && (control == tagName(i))) {
      i += 1
    }
    i
  }

  /** Split the tagname between controls and text.
    *
    * This method separates controls at the start of the tagname from
    * any text at the end.
    * 
    * Either side may return empty.
    */
  def splitTagControls()
      : (String, String) =
  {
    tagName.splitAt(tagControlPrefixCount)
  }


  override def toString
      : String =
  {
    val b = new StringBuilder("MarkAttributes(")
    b ++= "control:"
    b append control
    b ++= ", tagName:"
    b append tagName
    b ++= ", klass:"
    b append klass
    b ++= ", text:"
    b append text
    b ++= ", url:"
    b append url
    b ++= ", resolvedTagname:"
    b append resolvedTagname
    b += ')'
    b.result()
  }

}//MarkAttributes



object MarkAttributes {
  // NB: Woul;d need to override every allocation to return
  // new instances to make this work
  //private val emptyThing: MarkAttributes = new MarkAttributes('\0', "", "", "", "")
  //def empty(): MarkAttributes = emptyThing

  def apply()
      : MarkAttributes =
  {
    new MarkAttributes('\0', "", "", "", "")
  }

  def apply(
    control: Char,
    tagName: String,
    klass: String,
    text: String,
    url: String
  )
      : MarkAttributes =
  {
    new MarkAttributes(control, tagName, klass, url, text)
  }

}//MarkAttributes
