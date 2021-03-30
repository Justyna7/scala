object Main extends App {
// Zadanie 1.
// Korzystając z metod drop i take zdefiniuj funkcję:
// def subSeq[A](seq: Seq[A], begIdx: Int, endIdx: Int): Seq[A] = /* ... */
// która zwraca podciąg ciągów sekwencji seq z przedziału od indeksu begIdx do endIdx.

def subSeq[A](seq: Seq[A], begIdx: Int, endIdx: Int): Seq[A] ={
  seq.take(endIdx).drop(begIdx)
}
println(subSeq(Seq(1,7,10,5,3,7,2,8),3,5))
// Zadanie 2.
// Korzystając z metody foldLeft/foldRight i zdefiniuj generyczną funkcję:
// def deStutter[A](seq: Seq[A]): Seq[A] = /* ... */
// która usunie z sekwencji seq wszystkie powtarzające się ciągi.
// Przykład:
// Dla: seq = Seq(1, 1, 2, 4, 4, 4, 1, 3), funkcja powinna zwrócić: Seq(1, 2, 4, 1, 3).



// Zadanie 3.
// Korzystając z metod filter, map i zipWithIndex zdefiniuj funkcję:
// def remElems[A](seq: Seq[A], k: Int): Seq[A] = /* ... */
// która usunie k-ty element sekwencji seq.



// Zadanie 4.
// Korzystając z metody groupBy zdefiniuj funkcję:
// def freq[A](seq: Seq[A]): Set[(A, Int)] = /* ... */
// która zwróci częstość wystąpienia poszczególnych elementów w ciągu set.
// Przykład:
// Dla set = Set('a','b','a','c','c','a') funkcja powinna zwrócić Set(('a', 3),('b', 1),('c', 2)).



// Zadanie 5.
// Korzystając z metod sliding, map, foldLeft/foldRight zdefiniuj funkcję:
// def isOrdered[A](seq: Seq[A])(leq:(A, A) => Boolean): Boolean = /* ... */
// która zwróci informację czy wszystkie sąsiednie elementy w seq, są zgodne z predykatem leq.
// Przykłady:
// Dla seq = Seq(1, 2, 2, 4) i (_ < _) funkcja powinna zwrócić false.
// Dla seq = Seq(1, 2, 2, 4) i (_ <= _) funkcja powinna zwrócić true.



// Zadanie 6.
// Korzystając z metod oferowanych przez kolekcje iterowalne (Iterable[A]) napisz funkcję
// def countChars(str: String): Int
// która wylicza ile różnych znaków, użyto w napisie str.
// Podpowiedź: pomyśl o wykorzystaniu jednej z metod konwersji.


}
