object Zad2 {
// Zadanie 2.
// Korzystając z mechanizmów udostępnianych przez kolekcje oraz danych z pliku nazwiska.txt,
// spośród osób, których imiona składają się z maksymalnie dużej liczby różnych liter wyszukaj te,
// których nazwiska mają minimalną długość.
  def main(args: Array[String]): Unit = {
    val linie = io.Source
      .fromResource("nazwiska.txt")
      .getLines.toList.map(x => x.split(' ').toList)
    val maxi = linie.map(x => (x(0).toLowerCase().distinct.length)).max
    val linie2 = linie.filter(x => x(0).toLowerCase().distinct.length == maxi)
    val mini = linie2.map(x => (x(1).length)).min
    val linie3 = linie2.filter(x => x(1).length == mini).map(x => x.mkString(" "))
      println(linie3)
  }

}
