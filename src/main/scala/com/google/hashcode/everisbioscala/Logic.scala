package com.google.hashcode.everisbioscala

import scala.annotation.tailrec
import scala.collection.parallel.immutable.ParSeq

object Logic {

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
  def libraryScore(books: List[Book], signupTime: Int): Double = {
    val totalBookScore: Int = books
      .map(_.score)
      .sum

    //TODO: Es posible que se pueda mejorar la formula
    totalBookScore / signupTime.toDouble //Si hay librerias con pocos tiempo de levantado y otra con la misma puntuacion con mas tiempo, priorizamos la de menos tiempo que nos dara puntos antes
  }

  // Filtra por aquellas librerias que no se puedan dar de alta en el tiempo establecido y ordena.
  // Se queda con los libros por libreria que dara tiempo a evaluar y ordena
  
  def evalLibraries(librariesRest: List[Library], numDaysRest: Int): List[Library] = { // Es un stream por que solo queremos el primer caso
    val libraries: ParSeq[Library] = librariesRest.par
    libraries
      .filter(_.signupTime < numDaysRest) //Elimina Duplicados libros
      .map(library => library.copy(
        books = booksFilterAndOrder(if (library.withDuplicates) library.books.distinct
                                    else library.books, library.signupTime, library.scanPerDay, numDaysRest),
        withDuplicates = false,
        apparentScore = libraryScore(library.books, library.signupTime)))
      .toList
      .sortBy(-_.apparentScore)
  }

  //Funcion que recibe una lista de librerias, se queda con la primera y recomputa el resto teniendo en cuenta
  //el tiempo que ha pasado
  @tailrec
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
            //libr.filter(_.id != firstLib.id)
              .map(library => library.copy(books = library.books.filter(book => !(firstLib.books contains book))))
            , numDiasRestantes, acc :+ firstLib)
      }
    }
  }
}
