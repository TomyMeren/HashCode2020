package com.google.hashcode.everisbioscala

object Logic {

  def finalSolution(a:ReaderFirstPhase):ReaderFirstPhase = {

    val numBooks = a.numBooks
    val numLibraries = a.numLibraries
    val numDays = a.numDays
    val librerias = a.libraries

    def scoreCalc(libros: List[Book], paralelismo: Int, tiempoLevantarse: Int): Int = {
      libros
        .sortBy(-_.score)
        .flatMap {
          libro => (tiempoLevantarse until libros.length)
            .map(indice => (libro, indice))
        }
        .filter(_._2 <= paralelismo * numDays)
        .map(_._1.score)
        .sum
    }

    //case class Library(id: Int, books: List[Book], signupTime: Int, scanPerDay: Int)

    val libraries: List[Library] = librerias
      .filter(_.signupTime < numDays)
      .sortBy(x => - scoreCalc(x.books, x.scanPerDay, x.signupTime) * (x.scanPerDay / x.signupTime))
      .map(x => Library(x.id, x.books.sortBy(-_.score), x.signupTime, x.scanPerDay))

    ReaderFirstPhase(numBooks,numLibraries,numDays,libraries)
  }
}
