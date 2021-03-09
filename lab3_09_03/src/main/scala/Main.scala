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
}
