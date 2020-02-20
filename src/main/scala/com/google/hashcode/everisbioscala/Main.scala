package com.google.hashcode.everisbioscala

object Main extends App {
  val a: ReaderFirstPhase = InputParser.readFile("input.txt")

  val numBooks = a.numBooks
  val numLibraries = a.numLibraries
  val numDays= a.numDays
  val librerias = a.libraries

  def calculoPuntuacion(libros:List[Book],paralelismo:Int,tiempoLevantarse:Int):Int = {
    libros
      .sortBy( - _.score)
      .flatMap {
      libro => (tiempoLevantarse to libros.length).map(indice => (libro,indice))
    }
      .filter(_._2 <= paralelismo * numDays)
      .map(_._1.score)
      .reduce(_ + _)
  }

  //case class Library(id: Int, books: List[Book], signupTime: Int, scanPerDay: Int)

  val libraries:List[Library] = librerias
    .filter(_.signupTime < numDays)
    .sortBy(x=> calculoPuntuacion(x.books,x.scanPerDay,x.signupTime) * (x.scanPerDay /x.signupTime))
    .map(x=> Library(x.id,x.books.sortBy(- _.score),x.signupTime,x.scanPerDay))


//case class ReaderFirstPhase(numBooks: Int, numLibraries: Int, numDays: Int, libraries: List[Library])
  val z: ReaderFirstPhase = ReaderFirstPhase(numBooks,numLibraries,numDays,libraries)
  OutputWriter.writeOutput(z, "output.txt")
}
