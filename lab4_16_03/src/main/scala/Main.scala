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

def tasuj2(l1: List[Int], l2: List[Int]): List[Int]={
  def poboczna(l1: List[Int], l2: List[Int], w: List[Int]): List[Int] = (l1,l2,w) match {
    
    case (List(), List(), x) => x.reverse

    case (List(), a::b, x::_) if x == a => poboczna(List(), b, w)
    case (List(), a::b, _) => poboczna(List(), b, a::w)
    
    case (a::b, List(), x::_) if x == a => poboczna(b, List(), w)
    case (a::b, List(), _) => poboczna(b, List(), a::w)
    
    case (a::b, c::d, x::_) if (c > a && x == a) => poboczna(b, c::d, w)
    case (a::b, c::d, _) if c > a => poboczna(b,c::d, a::w)
    
    case (a::b, c::d, x::_) if (c == a && x == a) => poboczna(b, d, w)
    case (a::b, c::d, _) if c == a => poboczna(b,d, a::w)
    
    case (a::b, c::d, x::_) if x == c => poboczna(a::b, d, w)
    case (a::b, c::d, _) => poboczna(a::b,d, c::w)
  }
  poboczna(l1,l2,List())
}
println(tasuj2(List(2, 4, 3, 5), List(1, 2, 2, 3, 1, 5)))

// Zadanie 3.
// Zdefiniuj generyczną funkcję rekurencyjną:
// def usun[A](l: List[A], el: A): List[A] = /* ... */
// która "usunie" z listy l wszystkie wystąpienia elementu el.
// Rozwiąż to zadanie wykorzystując rekurencję ogonową i dopasowanie wzorców (nie używaj metod head i tail).
// Przykład:
// Dla: usun(List(2, 1, 4, 1, 3, 3, 1, 2), 1), funkcja powinna zwrócić: List(2, 4, 3, 3, 2).
def usun[A](l: List[A], el: A): List[A] ={
  def poboczna[A](l: List[A], el: A, wynik: List[A]):List[A] = l match{
    case List() => wynik.reverse
    case a::b if a == el => poboczna(b,el,wynik)
    case a::b => poboczna(b,el,a::wynik)
  }
  poboczna(l,el,List[A]())
}
println(usun(List('A','b','e','C','d'),'e'))

// Zadanie 4.
// Zdefiniuj generyczną funkcję rekurencyjną:
// def divide[A](l: List[A]): (List[A], List[A]) = /* ... */
// która podzieli listę l na dwa elementy.
// W pierwszej będą się znajdywać elementy na parzystych indeksach w drugiej elementy na nie parzystych.
// Rozwiąż to zadanie wykorzystując rekurencję ogonową i dopasowanie wzorców (nie używaj metod head i tail).
// Przykład:
// Dla: List(1, 3, 5, 6, 7), funkcja powinna zwrócić: (List(1, 5, 7), List(3, 6)).
def divide[A](l: List[A]): (List[A], List[A]) ={
  def poboczna[A](l: List[A], a: List[A], b: List[A]): (List[A], List[A]) = l match{
    case List() => (a.reverse,b.reverse)
    case x::y => y match{
      case i::j => poboczna(j, x::a,i::b)
      case _ => poboczna(y,x::a,b)
    }
  }
  poboczna(l,List[A](),List[A]())
}
println(divide(List('A','b','e','C','d')))
println(divide(List('A','b','e','C')))
println(divide(List()))
}
