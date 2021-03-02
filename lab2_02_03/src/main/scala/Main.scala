object Main extends App {
  // println("Jeśli koniecznie chcemy, to w Scali możemy też pisać w stylu XIX-wiecznym:")
  // print("Podaj wartość y: ")
  // // Wczytujemy liczbę całkowitą i zapamiętujemy ją jako „wartość” max
  // // Użycie wartości zamiast zmiennej to drobne odstępstwo od stylu XIX-wiecznego ;)
  // val max = io.StdIn.readInt()
  // // deklarujemy i inicjalizujemy zmienną x
  // var x = 1
  // // drobna próbka „interpolacji” w wypisywanym napisie
  // println(s"Liczby z przedziału [1..$max] to:")
  // // w „pętli” zmieniamy wartość zmiennej aż będzie ona równa „max”
  // while (x <= max) {
  //   println(x)
  //   x += 1
  // }


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
}
