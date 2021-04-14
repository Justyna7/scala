object Main extends App {
// Zadanie 1. Korzystając z metod oferowanych przez kolekcje zdefiniuj funkcję:
// def indices[A](seq: Seq[A], el: A): Set[Int] = /* ... */
// która zwróci wszystkie indeksy w ciągu seq, na których znajduje się element el.
// Przykład:
// Dla: seq = Seq(1, 2, 1, 1, 5), el = 1, funkcja powinna zwrócić: Set(0, 2, 3).
// Dla: seq = Seq(1, 2, 1, 1, 5), el = 7, funkcja powinna zwrócić: Set().
def indices[A](seq: Seq[A], el: A): Set[Int] = {
  seq.zipWithIndex.filter(x => (x._1 ==el)).foldLeft(List[Int]())((a,b) => b._2::a).toSet
}
val n = Seq(1, 2, 1, 1, 5)
println(indices(n,1))
// Zadanie 2. Korzystając z metod oferowanych przez kolekcje zdefiniuj funkcję:
// def swap[A](seq: Seq[A]): Seq[A] = /* ... */
// która zamieni kolejnością wartości znajdujących się na parzystych i nieparzystych indeksach.
// Przykład:
// Dla: seq = Seq(1, 2, 3, 4, 5), Seq(2, 1, 4, 3, 5).
def swap[A](seq: Seq[A]): Seq[A] = {
  seq.grouped(2).foldLeft(List[Seq[A]]())((x,y)=> y :: x).flatten.reverse.toSeq
}
println(swap(Seq(1, 2, 3, 4, 5)))
// Zadanie 3. Korzystając z metod oferowanych przez kolekcje zdefiniuj funkcję:
// def minNotContained(set: Set[Int]): Int = /* ... */
// która zwróci najmniejszą nieujemną liczbę całkowitą, która nie występuje w zbiorze set.
// Przykład:  
// Dla: set = Set(-3, 0, 1, 2, 5, 6), funkcja powinna zwrócić: 3.
//def minNotContained(set: Set[Int]): Int =
println(Set(-3, 0, 1, 2, 5, 6).filter(_>=0).toList.sorted.foldLeft(0)((x,y)=> if(x == y) x+1 else x))

// Zadanie 4. Korzystając z ciągu wszystkich stref czasowych (postaci Kontynent/Strefa):
// val strefy: Seq[String] = java.util.TimeZone.getAvailableIDs.toSeq
// oraz operacji na ciągach i zasugerowanej we wskazówce operacji stripPrefix na napisach,
// wyszukaj strefy znajdujące się w Europie i posortuj rosnąco ich nazwy względem długości.
// Strefy, których nazwy mają taką samą długość posortuj w kolejności alfabetycznej.
// Podpowiedź: wykorzystaj między innymi metody: map, filter oraz standardową operację na napisach:
// def stripPrefix(prefix: String): String
// która usuwa podany prefiks z napisu, np.
// "ala ma kota".stripPrefix("ala ") -> "ma kota"

// Zadanie 5. Gra MaterMind polega na odgadnięciu w jakich miejscach zostały umieszczone n ukrytych kul,
// które są oznaczone powtarzającymi się kolorami.
// Gracz wygrywa, jeżeli w określonej liczbie ruchów odgadnie w jakich miejscach zostały umieszczone kule.
// W każdej turze gracz wybiera n kul, po czym zostaje mu wyświetlona informacja czy trafił.
// Każda prawidłowo odgadnięta kula (kula o właściwym kolorze na właściwym miejscu) sygnalizowana jest czarną kropką.
// Natomiast jeżeli gracz odgadł kolor kuli, ale nie odgadł jej lokalizacji, jest to sygnalizowane białą kropką.
// Gracz nie wie, które kule są właściwe, które zaś nie.
// Korzystając z metod oferowanych przez kolekcję zdefiniuj metodę oceniania ruchów dla gry MaterMind,
// czyli zwraca liczbę białych i czarnych kropek.
// Przykład:
// def score(code: Seq[Int])(move: Seq[Int]): (Int, Int)
// Przykład:
// val code = Seq(1, 3, 2, 2, 4, 5)
// val move = Seq(2, 1, 2, 4, 7, 2)
// Funkcja powinna zwrócić: (1, 3)
}
