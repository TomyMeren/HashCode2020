package com.google.hashcode.everisbioscala

object Main extends App {
  val a: ReaderFirstPhase = InputParser.readFile("src/main/resources/a_example.txt")
  val b: ReaderFirstPhase = InputParser.readFile("src/main/resources/b_read_on.txt")
  val c: ReaderFirstPhase = InputParser.readFile("src/main/resources/c_incunabula.txt")
  val d: ReaderFirstPhase = InputParser.readFile("src/main/resources/d_tough_choices.txt")
  val e: ReaderFirstPhase = InputParser.readFile("src/main/resources/e_so_many_books.txt")
  val f: ReaderFirstPhase = InputParser.readFile("src/main/resources/f_libraries_of_the_world.txt")


  def solucionFinal(a:ReaderFirstPhase):ReaderFirstPhase = {

    val numBooks = a.numBooks
    val numLibraries = a.numLibraries
    val numDays = a.numDays
    val librerias = a.libraries

    def ordenaLibros (libros: List[Book], paralelismo: Int, tiempoLevantarse: Int): List[(Book,Int)] = {
      libros
        .sortBy(-_.score)
        .flatMap {
          libro => (tiempoLevantarse until libros.length)
            .map(indice => (libro, indice))
        }
        .filter(_._2 <= paralelismo * numDays)
    }

    def calculoPuntuacion(libros: List[(Book,Int)]): Int = {
      libros
        .map(_._1.score)
        .reduce(_ + _)
    }

    //case class Library(id: Int, books: List[Book], signupTime: Int, scanPerDay: Int)

    val libraries: List[Library] = librerias
      .filter(_.signupTime < numDays)
      .sortBy(x => - calculoPuntuacion(ordenaLibros(x.books, x.scanPerDay, x.signupTime)) * (x.scanPerDay / x.signupTime))
      .map(x => Library(x.id,
        ordenaLibros(x.books
        .toSet.toList
        .sortBy(-_.score),x.scanPerDay, x.signupTime)
          .map(_._1),
         x.signupTime, x.scanPerDay))

    ReaderFirstPhase(numBooks,numLibraries,numDays,libraries)
  }

//case class ReaderFirstPhase(numBooks: Int, numLibraries: Int, numDays: Int, libraries: List[Library])
  val a_output: ReaderFirstPhase = solucionFinal(a)
  val b_output: ReaderFirstPhase = solucionFinal(b)
  val c_output: ReaderFirstPhase = solucionFinal(c)
  val d_output: ReaderFirstPhase = solucionFinal(d)
  val e_output: ReaderFirstPhase = solucionFinal(e)
  val f_output: ReaderFirstPhase = solucionFinal(f)

  OutputWriter.writeOutput(a_output, "src/main/resources/a_output.txt")
  OutputWriter.writeOutput(b_output, "src/main/resources/b_output.txt")
  OutputWriter.writeOutput(c_output, "src/main/resources/c_output.txt")
  OutputWriter.writeOutput(d_output, "src/main/resources/d_output.txt")
  OutputWriter.writeOutput(e_output, "src/main/resources/e_output.txt")
  OutputWriter.writeOutput(f_output, "src/main/resources/f_output.txt")


}
