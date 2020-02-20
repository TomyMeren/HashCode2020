package com.google.hashcode.everisbioscala

import scala.io.Source

class InputParser {
  def readFile(path: String): ReaderFirstPhase  = {
    val input: List[String] = Source.fromFile(path).getLines().toList
    val firstLine = readFirstLine(input.head)
    val secondLine = readSecondLine(input.tail.head)
    val restLines = readRestLines(input.tail.tail)

    ???
  }

  def readFirstLine(line: String): FirstLine = {
    val input = line.split(" ").toList.map(_.toInt)
    val (numBooks, tail1) = (input.head, input.tail)
    val (numLibraries, tail2) = (tail1.head, tail1.tail)
    val numDays = tail2.head
    FirstLine(numBooks, numLibraries, numDays)
  }

  def readSecondLine(line: String): SecondLine = {
    val input = line.split(" ").toList.map(_.toInt)
    val indexed: List[(Int, Int)] = input.zipWithIndex
    SecondLine(indexed)
  }

  def readRestLines(lines: List[String]) = ???
}
