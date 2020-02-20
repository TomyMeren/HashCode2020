package com.google.hashcode.everisbioscala

import org.scalatest._
import org.scalatest.matchers.should.Matchers


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

}
