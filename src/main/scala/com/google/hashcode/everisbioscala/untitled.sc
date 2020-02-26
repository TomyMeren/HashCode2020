import scala.math.BigInt._

case class Book(id: Int, score: Int)
case class Library(id: Int, books: List[Book], signupTime: Int, scanPerDay: Int)
case class ReaderFirstPhase(numBooks: Int, numLibraries: Int, numDays: Int, libraries: List[Library])

val librerias: List[Library]
val libros1: List[Book] = List(book(1))
val libros2: List[Book]
val libros3  : List[Book]


//Ordena una lista de libros sin duplicados y elimina aquellos que no entran en el periodo de tiempo establecido
def booksFilterAndOrder(books: List[Book], signupTime: BigInt, scanPerDay: BigInt, numDaysRest: BigInt): List[Book] = {

  val numBooksInTime: BigInt = scanPerDay * (numDaysRest - signupTime)

  val outputBooks = books
    .sortBy(-_.score)
    .take(if (!numBooksInTime.isValidInt) books.length else numBooksInTime.toInt)

  outputBooks
}

//Devuelve la puntuacion de cada libreria
def libraryScore(books: List[Book], signupTime: Int, scanPerDay: Int, numDaysRest: Int): Double = {
  val totalBookScore: Int = books
    .map(_.score)
    .sum

  //TODO: Es posible que se pueda mejorar la formula
  totalBookScore / signupTime.toDouble
}

// Filtra por aquellas librerias que no se puedan dar de alta en el tiempo establecido y ordena.
// Se queda con los libros por libreria que dara tiempo a evaluar y ordena
def evalLibraries(librariesRest: List[Library], numDaysRest: Int): List[Library] = { // Es un stream por que solo queremos el primer caso
  librariesRest
    .filter(_.signupTime < numDaysRest)
    .map(library => Library(
      library.id,
      booksFilterAndOrder(library.books.toSet.toList, library.signupTime, library.scanPerDay, numDaysRest), //Elimina Duplicados libros
      library.signupTime,
      library.scanPerDay))
    .sortBy(library => -libraryScore(library.books, library.signupTime, library.scanPerDay, numDaysRest))
}

//Funcion que recibe una lista de librerias, se queda con la primera y recomputa el resto teniendo en cuenta
//el tiempo que ha pasado
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
            .map(library => Library(
              library.id,
              library.books.filter(book => !(firstLib.books contains book)),
              library.signupTime,
              library.scanPerDay)), numDiasRestantes, acc :+ firstLib)
    }
  }
}
