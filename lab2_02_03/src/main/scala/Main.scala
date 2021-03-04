object Main extends App {
  // Zad 1
  // Napisz program, który wczytuje ze standardowego wejścia liczbę naturalną (patrz Uwaga poniżej),
  // a następnie sprawdza czy jest ona liczbą pierwszą i wyświetla odpowiednią informację.
  // Rozwiąż to zadanie bez korzystania ze zmiennych i "pętli".
  // Do wczytywania liczb całkowitych ze standardowego wejścia użyj io.StdIn.readInt().

  def czy_pierwsza(n:Int): String = {
    def poboczna(n:Int, dziel:Int): String ={
      n match{
        case 0 => "Nie pierwsza"
        case 1 => "Pierwsza"
        case _ => poboczna(n%dziel,dziel+1)
      }
    }
    n match{
      case 0 => "Nie pierwsza"
      case 1 => "Nie pierwsza"
      case 2 => "Pierwsza"
      case _ => poboczna(n%2, 3)
    }
  }
  val liczba = io.StdIn.readInt()
  println(czy_pierwsza(liczba))

// Zad 2
// Napisz funkcję rekurencyjną jestPalindromem(tab: Array[Int]): Boolean,
// która sprawdza czy tablica będąca jej argumentem jest palindromem.
// Dostęp do elementu tablicy tab o indeksie i uzyskujemy przez tab(i).
// Elementy tablicy mają indeksy od 0 do tab.size - 1.
// Przykładowa tablica, która jest palindromem to Array(1,2,3,2,1).
// Rozwiąż to zadanie bez korzystania ze zmiennych i "pętli".

def jestPalindromem(tab: Array[Int]): Boolean = {
  @annotation.tailrec
    def poboczna(tab: Array[Int], i: Int, pol: Int, s: Int): Boolean = {
      if (tab(i) == tab(s-i-1)){
        if (i == pol) true
        else poboczna(tab, i+1, pol, s)
      }else false
    }
    if (tab.length <= 1) true
    else {
      val s = tab.length
      poboczna(tab, 0, s/2, s)
    }
  }
val palindrom = Array(1)
val palindrom2 = Array(1,2)
val palindrom3 = Array(1,1)
val palindrom4 = Array(1,2,1)
val palindrom5 = Array(1,2,2,1)
val palindrom6 = Array(1,2,4,1)

println(jestPalindromem(palindrom))
println(jestPalindromem(palindrom2))
println(jestPalindromem(palindrom3))
println(jestPalindromem(palindrom4))
println(jestPalindromem(palindrom5))
println(jestPalindromem(palindrom6))
}
