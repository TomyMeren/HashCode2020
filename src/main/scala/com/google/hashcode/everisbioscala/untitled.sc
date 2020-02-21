case class Book(id: Int, score: Int)
case class Library(id: Int, books: List[Book], signupTime: Int, scanPerDay: Int)

def libraryScore(books: List[Book], signupTime: Int, scanPerDay: Int, numDaysRest: Int): Double = {
  val totalBookScore: Int = books
    .map(_.score)
    .sum
  //indUp => Da cuenta de que el numero de dias en levantarse es mas relevante cuando el tiempo de ejecucion es menor
  val indUp: Double = signupTime.toDouble / numDaysRest.toDouble

  (totalBookScore * scanPerDay) / indUp //TODO: Es posible que se pueda mejorar la formula
}


//val libros: List[Book] = List(Book(1, 5), Book(2, 4), Book(3, 1), Book(4, 23), Book(5, 8), Book(6, 14), Book(7, 24))
val libros: List[Book] = List(Book(7, 24), Book(4, 23), Book(6, 14), Book(5, 8))

val current =libraryScore(libros,4,2,6)
val expected = List(Book(7, 24), Book(4, 23), Book(6, 14), Book(5, 8))

current == (69 * 2) / (4.0 / 6.0)