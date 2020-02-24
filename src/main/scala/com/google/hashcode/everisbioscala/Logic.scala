package com.google.hashcode.everisbioscala

import scala.collection.parallel.immutable.ParSeq

object Logic {

  def filterSort(input: ReaderFirstPhase): ReaderFirstPhase = {
    val libraries: ParSeq[Library] = input.libraries.par
    val libsWithSortedBooks: ParSeq[Library] = for {
      lib <- libraries
      books = lib.books.sortBy(_.score)(Ordering[Int].reverse)
    } yield lib.copy(books = books)

    val libsWithSortedAndFilteredBooks: ParSeq[Library] = for {
      lib <- libsWithSortedBooks
      booksToScan: Int = (input.numDays - lib.signupTime) * lib.scanPerDay
      books: List[Book] = lib.books.take(booksToScan)
      apparentScore: Int = {
        for {
          b <- lib.books
        } yield b.score
      }.sum
    } yield lib.copy(books = books, apparentScore = apparentScore)

    val sortedLibs = libsWithSortedAndFilteredBooks
      .toList
      .sortBy(_.apparentScore)(Ordering[Int].reverse)

    input.copy(libraries = sortedLibs)
  }

  
  def finalSolution(a: ReaderFirstPhase): ReaderFirstPhase = {
    val numDaysIni: Int = a.numDays
    val librariesIni: List[Library] = a.libraries

    ReaderFirstPhase(a.numBooks, a.numLibraries, numDaysIni, loop(librariesIni, numDaysIni))
  }

  //Ordena una lista de libros sin duplicados y elimina aquellos que no entran en el periodo de tiempo establecido
  def booksFilterAndOrder(books: List[Book], signupTime: Int, scanPerDay: Int, numDaysRest: Int): List[Book] = {
    val numBooksInTime: Int = scanPerDay * (numDaysRest - signupTime)

    books
      .sortBy(-_.score) //TODO: Mejorable: solo cambia el numero de libros que coge y no es necesario reconputar el sort
      .take(numBooksInTime)
  }

  //Devuelve la puntuacion de cada libreria
  def libraryScore(books: List[Book], signupTime: Int, scanPerDay: Int, numDaysRest: Int): Double = {
    val totalBookScore: Int = books
      .map(_.score)
      .sum
    //indUp => Da cuenta de que el numero de dias en levantarse es mas relevante cuando el tiempo de ejecucion es menor
    val indUp: Double = signupTime.toDouble / numDaysRest.toDouble

    (totalBookScore * scanPerDay) / indUp //TODO: Es posible que se pueda mejorar la formula
  }

  // Filtra por aquellas librerias que no se puedan dar de alta en el tiempo establecido y ordena.
  // Se queda con los libros por libreria que dara tiempo a evaluar y ordena

  def eval(librariesRest: List[Library], numDaysRest: Int): Stream[Library] = { // Es un stream por que solo queremos el primer caso

    librariesRest
      .filter(_.signupTime < numDaysRest)
      .map(library => Library(
        library.id,
        booksFilterAndOrder(library.books.toSet.toList, library.signupTime, library.scanPerDay, numDaysRest), //Elimina Duplicados libros
        library.signupTime,
        library.scanPerDay))
      .sortBy(library => libraryScore(library.books, library.signupTime, library.scanPerDay, numDaysRest))
      .toStream
  }

  //Funcion que recibe una lista de librerias, se queda con la primera y recomputa el resto teniendo en cuenta
  //el tiempo que ha pasado

  def loop(restLibr: List[Library], numDaysPasados: Int): List[Library] = {

    val result = eval(restLibr, numDaysPasados)

    if(result.isEmpty) Nil
    else {
      val firstLib: Library = eval(restLibr, numDaysPasados).head
      val numDiasRestantes: Int = numDaysPasados - firstLib.signupTime

      if (numDiasRestantes <= 0) List(firstLib)
        else firstLib :: loop(restLibr diff List(firstLib), numDiasRestantes)
    }
  }
}
