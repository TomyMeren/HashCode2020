package com.google.hashcode.everisbioscala

object HelloWorld extends App {

  case class ReaderFirstPhase(numBooks: Int, numLibraries: Int, numDays: Int, libraries: List[Library])
  case class Library(books: List[Book], signupTime: Int, scanPerDay: Int)
  case class Book(id: Int, score: Int)

  val numBooks = ReaderFirstPhase.numBooks
  val numLibraries = ReaderFirstPhase.numLibraries
  val numDays= ReaderFirstPhase.numDays
  val librerias = ReaderFirstPhase.libraries

  def calculoPuntuacion(libros:List[Book],paralelismo:Int,tiempoLevantarse:Int):Int = {
    libros
      .orderBy(_.score)
      .flatMap(libro => (tiempoLevantarse to libros.length).map(libro,indice))
      .filter(_._2 <= paralelismo * numDays)
      .sum
  }

  librerias
    .filter(_.signupTime < numDays)
    .map()

  (sum(puntuacionLibros) * scanPerDay) / signupTime
}
