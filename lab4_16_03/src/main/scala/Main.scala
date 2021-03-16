object Main extends App {
// Zadanie 1.
// Zdefiniuj funkcję rekurencyjną:
// def sumuj(l: List[Option[Double]]): Option[Double]
// która zwraca wartość Some(suma) gdzie suma oznacza sumę wszystkich elementów listy l postaci Some(d),
// gdzie d > 0 lub zwraca None, gdy elementów takich na liście l brak. Użyj rekurencji ogonowej i dopasowania wzorca.
// Przykład:
// Dla: sumuj(List(Some(2.0), Some(4.0), Some(-3.0), None, Some(-3.0), None, Some(1.0))) , funkcja powinna zwrócić: Some(7.0).
def sumuj(l: List[Option[Double]]): Option[Double] = {
  def poboczna(l: List[Option[Double]], w: Option[Double]): Option[Double] = (l,w) match{
    case (List(),Some(x)) if x == 0.0 => None
    case (List(),x) => x
    case (Some(a)::b, Some(x)) if (a > 0) => poboczna(b, Some(a+x))
    case (_::b,x) => poboczna(b,x)
  }
  poboczna(l, Some(0.0))
}
println(sumuj(List(Some(2.0), Some(4.0), Some(-3.0), None, Some(-3.0), None, Some(1.0))))
println(sumuj(List(None)))
println(sumuj(List()))
}
