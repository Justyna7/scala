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






}