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

    def calculoPuntuacion(libros: List[Book], paralelismo: Int, tiempoLevantarse: Int): Int = {
      libros
        .sortBy(-_.score)
        .flatMap {
          libro => (tiempoLevantarse to libros.length).map(indice => (libro, indice))
        }
        .filter(_._2 <= paralelismo * numDays)
        .map(_._1.score)
        .reduce(_ + _)
    }

    //case class Library(id: Int, books: List[Book], signupTime: Int, scanPerDay: Int)

    val libraries: List[Library] = librerias
      .filter(_.signupTime < numDays)
      .sortBy(x => calculoPuntuacion(x.books, x.scanPerDay, x.signupTime) * (x.scanPerDay / x.signupTime))
      .map(x => Library(x.id, x.books.sortBy(-_.score), x.signupTime, x.scanPerDay))

    ReaderFirstPhase(numBooks,numLibraries,numDays,libraries)
  }


//case class ReaderFirstPhase(numBooks: Int, numLibraries: Int, numDays: Int, libraries: List[Library])
  val z: ReaderFirstPhase = solucionFinal()
  OutputWriter.writeOutput(z, "output.txt")

  val a_output: ReaderFirstPhase = a
  val b_output: ReaderFirstPhase = b
  val c_output: ReaderFirstPhase = c
  val d_output: ReaderFirstPhase = d
  val e_output: ReaderFirstPhase = e
  val f_output: ReaderFirstPhase = f

  OutputWriter.writeOutput(a_output, "src/main/resources/a_output.txt")
  OutputWriter.writeOutput(b_output, "src/main/resources/b_output.txt")
  OutputWriter.writeOutput(c_output, "src/main/resources/c_output.txt")
  OutputWriter.writeOutput(d_output, "src/main/resources/d_output.txt")
  OutputWriter.writeOutput(e_output, "src/main/resources/e_output.txt")
  OutputWriter.writeOutput(f_output, "src/main/resources/f_output.txt")


}
