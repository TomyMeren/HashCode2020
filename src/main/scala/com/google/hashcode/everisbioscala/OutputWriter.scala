package com.google.hashcode.everisbioscala

import java.io.{BufferedWriter, File, FileWriter}

object OutputWriter {

  def writeOutput(output: ReaderFirstPhase, path: String): Unit = {
    val file = new File(path)
    val bw = new BufferedWriter(new FileWriter(file))
    bw.write(output.numLibraries) // total libraries

    for {
      library <- output.libraries
    } yield {
      bw.write(library.books.size) // id numBooks
      bw.write(library.books.foldLeft("")((acc, book) => acc + " " + book.id)) // books (ordered)
    }
    bw.close()
  }
}
