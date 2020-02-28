package com.google.hashcode.everisbioscala

import scala.io.Source
import scala.util.{Failure, Success, Try}

object InputParser {
  def readFile(path: String): ReaderFirstPhase  = {
    val input: List[String] = Source.fromFile(path).getLines().toList
    val firstLine = readFirstLine(input.head)
    val secondLine = readSecondLine(input.tail.head)
    val restLines = readRestLines(input.tail.tail)
    val merged: ReaderFirstPhase = mergeInfo(firstLine, secondLine, restLines)
    merged
  }

  def readFirstLine(line: String): FirstLine = {
    val input = line.split(" ").toList.map(_.toInt)
    val (numBooks, numLibraries, numDays) = (input(0), input(1), input(2))
    FirstLine(numBooks, numLibraries, numDays)
  }

  def readSecondLine(line: String): SecondLine = {
    val input = line.split(" ").toList.map(_.toInt)
    val indexed: List[(Int, Int)] = (0 to input.size).toList zip input
    SecondLine(indexed)
  }

  def readRestLines(lines: List[String]): RestLines = {

    def helper(libraryInfo: String, libraryBooks: String): (String, Int, Int, List[Int]) = {
      val infoSplit: List[String] = libraryInfo.split(" ").toList
      val (numBooks: String, signupTime: Int, scanPerDay: Int) = (infoSplit(0), infoSplit(1).toInt, infoSplit(2).toInt)
      val booksSplit: List[String] = libraryBooks.split(" ").toList
      (numBooks, signupTime, scanPerDay, booksSplit.map(_.toInt))
    }

    def groupListToTuple[A](input: List[A], acc: List[(A, A)] = List.empty[(A, A)]): List[(A, A)] = {
      input match {
        case (x :: y :: z) if acc.isEmpty => groupListToTuple(z, (x, y) +: List.empty[(A, A)])
        case (x :: y :: z) => groupListToTuple(z,  acc :+ (x, y))
        case _ => acc
      }
    }
    
    val result: List[(String, Int, Int, List[Int])]  = groupListToTuple(lines)
      .map(x => helper(x._1, x._2))// List[(String, String)]

    RestLines(result)
  }

  def mergeInfo(firstLine: FirstLine, secondLine: SecondLine, restLines: RestLines): ReaderFirstPhase = {
    val finalLibraryList: List[Library] = restLines
      .tuples // List[numBooks: String, signupTime: Int, scanPerDay: Int, books: List[Int]]
      .zipWithIndex
      .map{ x =>
        val ((numBooks: String, signupTime: Int, scanPerDay: Int, books: List[Int]), libraryId: Int) = x
        val bookList: List[Book] = books
        .map{ bookToFind: Int =>
          val result: (Int, Int) = secondLine.books.find(_._1 == bookToFind).get
          Book(result._1, result._2)
        }
        Library(libraryId, bookList, signupTime, scanPerDay)
      }

    ReaderFirstPhase(firstLine.numBooks, firstLine.numLibraries, firstLine.numDays, finalLibraryList)
  }
}
