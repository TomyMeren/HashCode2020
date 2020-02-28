package com.google.hashcode.everisbioscala

import scala.collection.parallel.immutable.ParSeq

object Logic2 {

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
      .sortBy(_.apparentScore)(Ordering[Double].reverse)

    input.copy(libraries = sortedLibs)
  }

}
