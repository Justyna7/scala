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

// Zadanie 2.
// Zdefiniuj funkcję rekurencyjną:
// def tasuj(l1: List[Int], l2: List[Int]): List[Int]
// która łączy ze sobą dwie listy liczb całkowitych z zachowaniem porządku.
// W szczególności oznacza to, że jeśli l1 i l2 będą uporządkowane to wynik również będzie uporządkowany.
// W wyniku nie powinny pojawiać się kolejno powtarzające się elementy.
// Rozwiąż to zadanie wykorzystując rekurencję ogonową i dopasowanie wzorców (nie używaj metod head i tail).
// Przykład:
// Dla: tasuj(List(2, 4, 3, 5), List(1, 2, 2, 3, 1, 5)), funkcja powinna zwrócić: List(1, 2, 3, 1, 4, 3, 5).
def tasuj(l1: List[Int], l2: List[Int]): List[Int]={
  def poboczna(l1: List[Int], l2: List[Int], w: List[Int]): List[Int] = (l1,l2) match {
    case (List(),List()) => w.reverse
    case (List(),a::b) => w match {
                                    case x::_ if x == a => poboczna(List(), b, w)
                                    case _ => poboczna(List(), b, a::w)
                                  }
    case (a::b,List()) => w match {
                                    case x::_ if x == a => poboczna(b, List(), w)
                                    case _ => poboczna(b, List(), a::w)
                                  }
    case (a::b, c::d) if (c>a) => w match {
                                    case x::_ if x == a => poboczna(b, c::d, w)
                                    case _ => poboczna(b,c::d, a::w)
                                  }
    case (a::b, c::d) if (c==a) => w match {
                                    case x::_ if x == a => poboczna(b, d, w)
                                    case _ => poboczna(b,d, a::w)
                                  }
    case (a::b, c::d) => w match {
                            case x::_ if x == c => poboczna(a::b, d, w)
                            case _ => poboczna(a::b,d, c::w)
                          }
  }
  poboczna(l1,l2,List())
}
println(tasuj(List(), List()))
println(tasuj(List(2, 4, 3, 5), List(1, 2, 2, 3, 1, 5)))



}
