object Zad1 {
// Zadanie 1.
// Korzystając z metod oferowanych przez kolekcje iterowalne (Iterable[A])
// spośród liczb zawartych w pliku liczby.txt wyszukaj te,
// których kolejne cyfry tworzą ciąg niemalejący, a ich suma jest nieparzysta.

  def main(args: Array[String]): Unit = {
    val linie = io.Source
      .fromResource("liczby.txt")
      .getLines.toList.filter(x => (x == x.sorted)).filter(x => (x.foldLeft(0)(_ + _)%2 == 1))
    println(linie)
  }
  

}
