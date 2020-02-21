package com.google.hashcode.everisbioscala

object Main extends App {

  val prefix = "src/main/resources/"
  val inputFiles = List("a_example.txt"/*, "b_read_on.txt", "c_incunabula.txt", "d_tough_choices.txt", "e_so_many_books.txt", "f_libraries_of_the_world.txt"*/)
  val outputFiles = List("a_output.txt"/*, "b_output.txt", "c_output.txt", "d_output.txt", "e_output.txt", "f_output.txt"*/)
  val filePaths: List[(String, String)] = inputFiles
    .map(prefix+_)
    .zip{
      outputFiles
        .map(prefix+_)
    }

  filePaths
    .foreach{ tuple =>
      val read = InputParser.readFile(tuple._1)
      val result = Logic.finalSolution(read)
      OutputWriter.writeOutput(result, tuple._2)
    }
}
