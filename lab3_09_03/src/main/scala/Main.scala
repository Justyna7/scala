object Main extends App {
  // Zadanie 1
  // Zdefiniuj rekurencyjną funkcję reverse(str: String): String, która zwróci odwrócony napis pobrany jako argument.
  // Rozwiąż to zadanie bez korzystania ze zmiennych oraz wykorzystaj rekurencję ogonową.
def reverse(str: String): String = {
  @annotation.tailrec
  def rev(str1: String, str2: String): String = {
    if (str1 =="") str2
    else {
      rev(str1.tail, str1.head.toString + str2)
    }
  }
  rev(str, "")
}
println(reverse("ala ma kota"))


// Zadanie 2
// Napisz funkcję jestPierwsza(n: Int): Boolean, która sprawdza, czy argument jest liczba pierwszą.
// Rozwiąż to zadanie bez korzystania ze zmiennych oraz wykorzystaj rekurencję ogonową.

def jestPierwsza(n: Int): Boolean = {
  @annotation.tailrec
  def poboczna(n: Int, dziel: Int): Boolean ={
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
println(jestPierwsza(13))

// Zadanie 3
// Zdefiniuj funkcję ciag(n: Int): Int, która zwróci n-ty wyrażony wzorem:
// F(0) = 1
// F(1) = 1
// F(n) = F(n-1) + F(n-2) dla n > 1
// Rozwiąż to zadanie bez korzystania ze zmiennych oraz wykorzystaj rekurencję ogonową.
// Pierwsze 10 wyrazów ciągu: 1, 1, 2, 3, 5, 8, 13, 21, 34, 55.

def ciag(n: Int): Int = {
  @annotation.tailrec
  def poboczna(n: Int, wyr1: Int, wyr2: Int):Int = n match {
    case 1 => wyr1 + wyr2
    case _ => poboczna(n-1, wyr2, wyr1+wyr2)
  }
  if (n == 0 || n == 1) 1
  else poboczna(n-2, 1, 1)
}
println(ciag(8))

// Zadanie 4
// Zdefiniuj funkcję rekurencyjną
// def maksimum(l1: List[Double], l2: List[Double]): List[Double]
// która porównuje elementy list będących jej argumentami i "po współrzędnych" produkuje listę składającą się z maksimów.
// Jeżeli, któraś lista jest dłuższa, jej "nadmiarowe" elementy powinny zostać dodane na koniec listy wynikowej. Przykład:
// maksimum(List(2.0, -1.6, 3.2, 5.4, -8.4), List(3.3, -3.1, 3.2, -4.1, -0.4, 5.5)) == List(3.3, -1.6, 3.2, 5.4, -0.4, 5.5)
// Rozwiąż to zadanie wykorzystując rekurencję ogonową oraz dopasowanie do wzorca (nie korzystaj z metod head i tail).

def maksimum(l1: List[Double], l2: List[Double]): List[Double] = {
  @annotation.tailrec
  def poboczna(l1: List[Double], l2: List[Double], wynik:List[Double]): List[Double] = {
    def dopisz(lista: List[Double], wynik: List[Double]): List[Double] = lista match {
      case List() => wynik.reverse
      case a::b => dopisz(b, a::wynik)
    }

    l1 match {
      case List() => l2 match {
                            case List() => wynik.reverse
                            case a::b => dopisz(b, a::wynik)
                          }
      case x::y => l2 match {
                            case List() => dopisz(y, x::wynik)
                            case a::b if (x>=a) => poboczna(y, b, x::wynik)
                            case a::b if (a>x) => poboczna(y, b, a::wynik)
                          }
    }
  }
poboczna(l1,l2, List())
}
val tab1 = List(10.7,2,3,3,4,30,1,5,79,2)
val tab2 = List(3,4,2,5,4,15,16,4,35,23.6,10,5)
val tab3 = List(2.7)
println(maksimum(tab3,tab2))



}