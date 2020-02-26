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
    val sol: List[Library] = loop(librariesIni, numDaysIni, Nil)

    ReaderFirstPhase(a.numBooks, sol.length, numDaysIni, sol)
  }

  //Ordena una lista de libros sin duplicados y elimina aquellos que no entran en el periodo de tiempo establecido
  def booksFilterAndOrder(books: List[Book], signupTime: BigInt, scanPerDay: BigInt, numDaysRest: BigInt): List[Book] = {

    val numBooksInTime: BigInt = scanPerDay * (numDaysRest - signupTime)

    val outputBooks = books
      .sortBy(-_.score)
      .take(if (!numBooksInTime.isValidInt) books.length else numBooksInTime.toInt)

    outputBooks
  }

  //Devuelve la puntuacion de cada libreria
  def libraryScore(books: List[Book], signupTime: Int, scanPerDay: Int, numDaysRest: Int): Double = {
    val totalBookScore: Int = books
      .map(_.score)
      .sum

    //TODO: Es posible que se pueda mejorar la formula
    totalBookScore / signupTime.toDouble
  }

  // Filtra por aquellas librerias que no se puedan dar de alta en el tiempo establecido y ordena.
  // Se queda con los libros por libreria que dara tiempo a evaluar y ordena
  def evalLibraries(librariesRest: List[Library], numDaysRest: Int): List[Library] = { // Es un stream por que solo queremos el primer caso
    librariesRest
      .filter(_.signupTime < numDaysRest)
      .map(library => Library(
        library.id,
        booksFilterAndOrder(library.books.toSet.toList, library.signupTime, library.scanPerDay, numDaysRest), //Elimina Duplicados libros
        library.signupTime,
        library.scanPerDay))
      .sortBy(library => -libraryScore(library.books, library.signupTime, library.scanPerDay, numDaysRest))
  }

  //Funcion que recibe una lista de librerias, se queda con la primera y recomputa el resto teniendo en cuenta
  //el tiempo que ha pasado
  def loop(libr: List[Library], numDaysPasados: Int, acc: List[Library]): List[Library] = {

    val result = evalLibraries(libr, numDaysPasados)

    result match {
      case Nil => acc
      case firstLib :: Nil => acc :+ firstLib
      case firstLib :: restLibr => {
        val numDiasRestantes: Int = numDaysPasados - firstLib.signupTime
        println("diasRestantes: " + numDiasRestantes)
        if (numDiasRestantes <= 0) acc :+ firstLib
        else
          loop(
            restLibr
              .map(library => Library(
                library.id,
                library.books.filter(book => !(firstLib.books contains book)),
                library.signupTime,
                library.scanPerDay)), numDiasRestantes, acc :+ firstLib)
      }
    }
  }
}
