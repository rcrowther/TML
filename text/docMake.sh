#!/bin/sh
exec scala "$0" "$@"
!#
// run with,
// ./docMake.sh

import java.nio.file.Files
import java.io.File
import java.nio.file.StandardCopyOption._

object DocMake extends App {
  println("compiling...")
val srcDir = "/home/rob/Code/scala/TML/text"
val srcFile = new File(srcDir)
val srcPath = srcFile.toPath
val dstFile = new File(srcDir + "/Doc")
val dstPath = dstFile.toPath
val entryNames = Seq("GUIDE")

dstPath.resolve("lib").toFile.mkdirs()
Files.copy(srcPath.resolve("lib/doc.css"), dstPath.resolve("lib/doc.css"))

entryNames.foreach{ e =>
val data = tml.FileReader(srcPath.resolve(e))
}

}
DocMake.main(args)
