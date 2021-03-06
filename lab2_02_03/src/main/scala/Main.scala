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
  // val liczba = io.StdIn.readInt()
  // println(czy_pierwsza(liczba))



  def czy_pierwsza2(n:Int): Boolean = {
    def poboczna(n:Int, dziel:Int): Boolean ={
      n match{
        case 0 => false
        case 1 => true
        case _ => poboczna(n%dziel,dziel+1)
      }
    }
    n match{
      case 0 => false
      case 1 => false
      case 2 => true
      case _ => poboczna(n%2, 3)
    }
  }


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

// Zad 3
// Napisz funkcję daSię(n: Int): Boolean,
// która dla argumentu n będącego liczbą naturalną (tzn. n >= 0) sprawdza,
// czy każdą parzystą liczbę naturalną z przedziału (2..n]
// da się przedstawić jako sumę dwóch liczb pierwszych.
// Dla każdej ze sprawdzanych liczb funkcja powinna wypisać w konsoli znaleziony "rozkład".
// Rozwiąż to zadanie bez korzystania ze zmiennych i "pętli".

def daSię(n: Int): Boolean = {
  @annotation.tailrec
  def poboczna (n: Int): Boolean = {
    @annotation.tailrec
    def pierwsza (n: Int, test: Int): String = {
      //println(test)
      if (test > n/2) "blad"
      else{
        if (czy_pierwsza2(test)){
          if(czy_pierwsza2(n-test)){
            n.toString + ": " + test.toString + " + " + (n-test).toString
          }else pierwsza(n, test+1)
        }else pierwsza(n, test + 1)
      }
    }
    if (n <=3) false
    else{
      if(n%2 == 0){
        println(pierwsza(n, 2))
        poboczna(n-1)
      }else{
        println(pierwsza(n-1, 2))
        poboczna(n-2)
      }
      
    }
  }
  if (n<4){
    println("Nie da sie, podaj liczbe > 3")
    return false
  }else{
    poboczna(n)
  }
}
daSię(1)
daSię(13)

// Zad 4
// Napisz funkcję rekurencyjną trójkąt(wys: Int): Unit,
// która wyświetla na ekranie trójkąt o wysokości n:
//         1
//       1   1
//     1   2   1
//   1   3   3   1
// 1   4   6   4   1
//       ...
// Jak widać liczby stojące na brzegach trójkąta są równe 1,
// a każda stojąca „wewnątrz” jest sumą dwóch stojących "na prawo" i "na lewo" od jej pozycji,
// ale w wierszu poprzednim.
// Rozwiąż to zadanie bez korzystania ze zmiennych i "pętli".

def trójkąt(wys: Int): Unit = {
  def poboczna (n: Int, od: Int, li: List[Int]):Unit = {
    def str (l1: List[Int], l2: List[Int]): List[Int] = l1 match{
      case List() => l2
      case a::b => b match {
                      case x::_ => str(b, (a+x)::l2)
                      case _ => str(b,1::l2)
                    }
      case _ => 1::l2
    }
    def rysuj (lista: List[Int], s:String):String = lista match{
      case List() => s
      case a::b => rysuj(b, s + "   " + a.toString)
      case _ => s
    }
    println(rysuj(li, "  "*(n)))
    if (n == 1) println("")
    else {
      val przechowaj = str(li, List(1))
      poboczna(n-1, od, przechowaj)
      //println("  "*(od-n) + "1" + "   "*(n-1) + " "* (n-2) + "1" )
    }
  }
  if (wys < 1) println("")
  else poboczna(wys, wys, List(1))
}
println("trojkat 0")
trójkąt(0)
println("trojkat 1")
trójkąt(1)
println("trojkat 29")
trójkąt(29)
}
