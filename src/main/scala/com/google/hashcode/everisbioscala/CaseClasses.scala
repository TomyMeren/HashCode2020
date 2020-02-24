package com.google.hashcode.everisbioscala

case class Time(days: Int) {
  def passDay = Time(days-1)
}

case class Book(id: Int, score: Int, worldWideUnits: Int = 0)

case class Library(id: Int, books: List[Book], signupTime: Int, scanPerDay: Int, apparentScore: Int = 0)

case class FirstLine(numBooks: Int, numLibraries: Int, numDays: Int)
case class SecondLine(books: List[(Int, Int)])
case class RestLines(tuples: List[(String, Int, Int, List[Int])]) // List[numBooks: String, signupTime: Int, scanPerDay: Int, books: List[Int]]

case class ReaderFirstPhase(numBooks: Int, numLibraries: Int, numDays: Int, libraries: List[Library])
