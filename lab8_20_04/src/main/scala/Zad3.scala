object Zad3 {
// Zadanie 3. Korzystając z mechanizmów udostępnianych przez kolekcje
// oraz fragmentu powieści "Ogniem i mieczem" Henryka Sienkiewicza,
// skonstruuj "histogram" pokazujący częstotliwość występowanie w tekście poszczególnych liter.
// Małe i wielkie litery traktuj jako identyczne.
// Pomiń występujące w tekście znaki interpunkcyjne (kropki, przecinki, myślniki, cudzysłowy itp),
// a także znaki nie będące literami.
// Rozwiązanie przedstaw w postaci funkcji
// def histogram(max: Int): Unit
// która przyjmuje argument max oznacza maksymalną szerokość histogramu
// (jeżeli liter jest więcej histogram nie powinien przekroczyć max).
// Przykład:
// a:***************************************
// ą:**********
// b:*****************
// c:**************
// ć:*******
// ...
// Podpowiedź: mogą się przydać metody standardowe,
// takie jak np. isLetter, toLowerCase, toUpperCase itp.)

  def main(args: Array[String]): Unit = {
    val linie = io.Source
      .fromResource("ogniem-i-mieczem.txt")
      .getLines.toList.map(x => x.toList).flatten.filter(x => x.isLetter).map(x => x.toString.toLowerCase)
      .groupBy(identity).map(x => (x._1,x._2.count(x => x == x))).toList
    def histogram(maks: Int): Unit = {
      val m = linie.foldLeft(0)((a,b) => if(b._2 > a) b._2 else a)
      val h = linie.sortBy(_._1).map(x => (x._1,((x._2.toDouble/m) * maks).toInt))
        .map(x => ""+ x._1 + ": " +"*"*x._2).map(x => println(x))
  }
   histogram(150)
  }
  
}
