package tml


import java.nio.file.Paths
import java.nio.file.Path
import java.nio.charset.Charset
import java.nio.charset.StandardCharsets

import java.io.File
import java.nio.file._
import java.nio.file.attribute.BasicFileAttributes
import java.nio.file.FileVisitOption
import java.io.{FileOutputStream, FileInputStream, InputStreamReader}
import java.io.IOException


//tml.Runner.main(Array("/home/rob/Code/scala/TML/test/htmlList", "/home/rob/Code/scala/TML/test/htmlList"))
//tml.Runner.main(Array("-g", "markdown", "kk", "/home/rob/Code/scala/TML/test/Markdown", "/home/rob/Code/scala/TML/test"))

//tml.Runner.main(Array("-h"))
//tml.Runner.main(Array("-help"))
//tml.Runner.main(Array("-version"))

// TODO need startup script to run without Scala
// TODO: Set '.' to cwd
object Runner
{


  val cwd : Path = Paths.get(".").toAbsolutePath().normalize()


  def filenameSplit(filenameStr: String)
      : (String, String) =
  {
    var i = filenameStr.indexOf('.')

    if (i != -1){
      filenameStr.splitAt(i)
    }
    else (filenameStr, "")
  }

  /** Apples a function f to files in a directory, matching digit-only filenames.
    *
    * Only descends one level. Ignores directory and symlink files (as
    * detected by Java). Also ignores files beginning with '.' or
    * ending in '~' (hidden files on many systems).
    */
  def dirForEach(p: Path, grammarExt: String)
      : Seq[File] =
  {

    if(!p.toFile.isDirectory()) {
      throw new Exception(s"file is not directory: ${p.toFile}")
    }
    val b = Seq.newBuilder[File]

    class Paths
        extends SimpleFileVisitor[Path]
    {

      override def visitFile(p: Path, attrs: BasicFileAttributes)
          : FileVisitResult =
      {
        val pStr = p.toString
        val filenameStr = p.getFileName().toString
        var (filename, extension) = filenameSplit(filenameStr)
        
        // ignore hidden files (prefix '.' suffix '~')
        // and extensions that match the grammar (already converted)
        // and directories/symlinks.
        if(
          filenameStr(0) != '.'
            && filenameStr.last != '~'
            && extension != grammarExt
            && Files.isRegularFile(p)
        )
        {
          b += p.toFile
        }

        FileVisitResult.CONTINUE
      }

    }

    val o = new java.util.HashSet[FileVisitOption]()
    val s = new Paths

    // try {
    Files.walkFileTree(p,o, 1, s)
    b.result
    //  }
    // catch {
    //   case e: Exception =>
    // }

  }



  /** Gather options and file extensions.
    */
  private val grammarListExt = Map(
    "html" -> "htm",
    "htmlCB" -> "htm",
    "markdown" -> "mk"
  )

  private val grammarList = grammarListExt.keys.toSeq



  ///////////////////////////
  // Main handled switches //
  ///////////////////////////

  /** Outputs help.
    * 
    * Can sub-categorise help using the task name.
    * 
    * @param taskName name of a task, causes help to
    *  specialise it's string output.
    */
  def printHelpShort()
  {
    val help = """Usage: tml <options> <source files> <destination>
   or: tml -help
   (<source files> can either be a file or directory. Ignores files starting
   with '.' or ending in '~'. <destination> must be a
   directory. If either option is omitted, the current working directory is
   used)
Converts TML marked files to other markups
"""
    print(help)

  }

  def printHelp()
  {
    printHelpShort()

    val helpOptions = """
 -g, --grammar    one of {html, htmlCB, markdown} 
 -e, --errors     explain what is being done (reports errors)
 -v, --verbose    explain what is being done (reports files)
 -h, -help        display help and exit
 -version         output version information and exit
"""
    print(helpOptions)
  }


  /** Outputs version and license info.
    */
  def printVersion()
  {
    println(
      "tml " + tml.Version.version
        + "\n"
        + tml.Version.licence
        + "\n"
        + "\n"
        + tml.Version.author
    )
  }


  //BufferedReader in
  //= new BufferedReader(new InputStreamReader(System.in));
  // execute
  def executeOne(
    grammar: String,
    src: File,
    dstRoot: Path,
    extension: String,
    errors: Boolean,
    verbose: Boolean
  )
  {
    if (!src.exists()) {
      error(s"File not found: ${src.getPath}")
    }
    else {
      val is = new FileInputStream(src)
      val isr = new InputStreamReader(is)
      val it = InputIterator.newlineGrind(isr)
      val sFileName = src.getName()
      val fn = filenameSplit(sFileName)._1
      val dst = dstRoot.resolve(fn + '.' + extension)
      val os = new FileOutputStream(dst.toFile)
      val ot = OutputTarget.toUF8Stream(os)

      if (verbose) println(src.getPath)

      grammar match {
        case "html" => {
          if (!errors) HTML(ot, it)
          else HTML.errorReport(ot, it)
        }
        case "markdown" => {
          if (!errors) Markdown(ot, it)
          else Markdown.errorReport(ot, it)
        }
        case "htmlCB" => {
          if (!errors) HTMLCodeblock(ot, it)
          else HTMLCodeblock.errorReport(ot, it)
        }
      }
    }
  }

  def error(msg: String)
  //: Nothing =
  {
    System.err.println("Error: " + msg)
  }

  def errorThenExit(msg: String)
  //: Nothing =
  {
    error(msg)
    System.exit(1)
  }



  //////////////////
  // Main helpers //
  //////////////////


  /** You asked a question?
    */
  def main(inputArgs: Array[String]){
    //println(s"args:")
    //args.foreach(println)

    if(inputArgs.size == 0) {
      // Special case, no message, goto help
      printHelp()
    }
    else {
      // Filter overriding show options
      val mainHandledOption: Option[String] = inputArgs.find{ arg =>
        Seq("-help", "-h", "-version").contains(arg)
      }

      if (mainHandledOption != None) {
        mainHandledOption.get match{
          case "-h" => printHelpShort()
          case "-help" => printHelp()
          case "-version" => printVersion()
        }

      }
      else {

        // split the switches from the files
        // (painful)
        var allowArg = false
        val splitPos =
          inputArgs.indexWhere((a) => {
            if (a(0) == '-') {
              allowArg = (a == "-g" || a == "-grammar")
              false
            }
            else {
              if(allowArg) {
                if(a.contains('/')) {
                  errorThenExit(s"path argument in switch parameter position arg:$a")
                }

                allowArg = false
                false
              }
              else true
            }
          })

        val (switches, argFiles) =
          if (splitPos == -1) {
            (inputArgs, Array[String]())
          }
          else inputArgs.splitAt(splitPos)


        //println(s"switches.length  ${switches.length}")
        //println(s"argFiles.length  ${argFiles.length}")

        // treat the options
        val grammar = {
          val x = switches.indexWhere((s) => {s == "-grammar" || s == "-g"})
          //println(s"grammar at $x")
          if(x != -1) {
            if(x > switches.length - 2) {
              errorThenExit("grammar switch must be followed by a named grammar")
            }

            val g = switches(x + 1)
            if (!grammarList.contains(g)) {
              errorThenExit(s"grammar switch must be followed by one of ${grammarList.mkString(",")}")
            }

            g
          }
          else "html"
        }

        val verbose = {switches.find((s) => {s == "-verbose" || s == "-v"}) != None}
        val errors = {switches.find((s) => {s == "-errors" || s == "-e"}) != None}



        // treat the files
        val files = argFiles.map(new File(_))

        //val paths = files.map(_.toPath)

        // src files
        val srcs =
          if (files.length > 0) {
            if (files(0).isDirectory()) {
              dirForEach(files(0).toPath, grammar)
            }
            else Seq(files(0))
          }
          else {
            dirForEach(cwd, grammar)
          }

        val dst =
          if (files.length > 1) {
            if (!files(1).isDirectory()) {
              errorThenExit(s"Second file parameter is not a directory: ${files(1).getPath}")
            }
            files(1).toPath

          }
          else cwd
        
        val ext = grammarListExt(grammar)

        srcs.foreach{
          executeOne(grammar, _, dst, ext, errors, verbose)
        }
      }
    }
  }

}//Runner

