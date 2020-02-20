package com.google.hashcode.everisbioscala

object Main extends App {
  val a: ReaderFirstPhase = InputParser.readFile("src/main/resources/a_example.txt")
  val b: ReaderFirstPhase = InputParser.readFile("src/main/resources/b_read_on.txt")
  val c: ReaderFirstPhase = InputParser.readFile("src/main/resources/c_incunabula.txt")
  val d: ReaderFirstPhase = InputParser.readFile("src/main/resources/d_tough_choices.txt")
  val e: ReaderFirstPhase = InputParser.readFile("src/main/resources/e_so_many_books.txt")
  val f: ReaderFirstPhase = InputParser.readFile("src/main/resources/f_libraries_of_the_world.txt")


  val a_output: ReaderFirstPhase = a
  val b_output: ReaderFirstPhase = b
  val c_output: ReaderFirstPhase = c
  val d_output: ReaderFirstPhase = d
  val e_output: ReaderFirstPhase = e
  val f_output: ReaderFirstPhase = f

  OutputWriter.writeOutput(a_output, "src/main/resources/a_output.txt")
  OutputWriter.writeOutput(b_output, "src/main/resources/b_output.txt")
  OutputWriter.writeOutput(c_output, "src/main/resources/c_output.txt")
  OutputWriter.writeOutput(d_output, "src/main/resources/d_output.txt")
  OutputWriter.writeOutput(e_output, "src/main/resources/e_output.txt")
  OutputWriter.writeOutput(f_output, "src/main/resources/f_output.txt")


}
