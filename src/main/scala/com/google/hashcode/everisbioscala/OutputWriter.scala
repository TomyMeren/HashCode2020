package com.google.hashcode.everisbioscala

import java.io.{BufferedWriter, File, FileWriter}

object OutputWriter {

  def writeOutput(output: ReaderFirstPhase, path: String): Unit = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(output.numLibraries.toString+"\n") // total libraries

    for {
      library <- output.libraries
    } yield {
      bw.write(library.books.size.toString+"\n") // id numBooks
      bw.write(library.books.map(_.id.toString).mkString(" ")+"\n") // books (ordered)
    }
    bw.close()
  }
}
