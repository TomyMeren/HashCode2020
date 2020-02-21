package com.google.hashcode.everisbioscala

import org.scalatest._

class InputParserTest extends FlatSpec with Matchers {

  "function readFirstLine" should "match the following output" in {
    val current = InputParser.readFirstLine("6 2 7")
    val expected = FirstLine(6, 2, 7)

    assert(expected == current)
  }

  "function readSecondLine" should "match the following output" in {
    val current = InputParser.readSecondLine("1 2 3 6 5 4")
    val expected = SecondLine(List((1, 0), (2, 1), (3, 2), (6, 3), (5, 4), (4, 5)))

    assert(expected == current)
  }

  "funcion booksFilterAndOrder" should "match the following output" in {

    val libros: List[Book] = List(Book(1, 5), Book(2, 4), Book(3, 1), Book(4, 23), Book(5, 8), Book(6, 14), Book(7, 24))
    //def booksFilterAndOrder(books: List[Book], signupTime: Int, scanPerDay: Int, numDaysRest: Int)
    val current =Logic.booksFilterAndOrder(libros,4,2,6)
    val expected = List(Book(7, 24), Book(4, 23), Book(6, 14), Book(5, 8))

    assert(expected == current)
  }
}
