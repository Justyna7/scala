import akka.actor._

// Zadanie 2. Zdefiniuj klasę
// class Element extends Actor { … }
// tak, żeby reagował na komunikaty
// case class Wstaw(n: Int)
// case class Usuń(n: Int)
// Pierwszy otrzymany komunikat Wstaw(n) powinien spowodować zmianę tożsamości na
// def korzeń(wartość: Int): Receive = { ... }
// (oczywiście wywołaną z wartością n). Kolejne komunikaty postaci Wstaw(n), 
// gdzie n == wartość powinny spowodować przejście do tożsamości
// def zPotomkami(wartość, kolejny: Set[ActorRef])
// W tej tożsamości kolejne komunikaty Wstaw(wartość) powinny dodawać kolejne elementy do zbioru potomkowie. 
// Liczba elementów zbioru potomkowie powiększona o jeden będzie zatem równa liczbie otrzymanych przez "korzeń" 
// komunikatów postaci Wstaw(wartość).
// Dążymy do sytuacji, w której – przykładowo – po otrzymaniu czterech komunikatów Wstaw(wartość 1), 
// dwóch Wstaw(wartość 2), trzech Wstaw(wartość 3) oraz jednego Wstaw(wartość 4) 
// "stan" systemu będzie wyglądał następująco:
// <obrazek>
// Element-y "stojące w korzeniach" pomijają komunikaty Wstaw(n) dla n != wartość. 
// Analogicznie pomijane są przez nie komunikaty Usuń(n) dla n != wartość.
// Jeśli "korzeń" otrzyma komunikat Usuń(wartość) to powinien zakończyć działanie jednego ze swoich potomków 
// wysyłając do niego sygnał PoisonPill, a następnie usunąć go ze zbioru potomkowie 
// (a tak naprawdę, przejść do nowej tożsamości). 
// Jeśli "korzeń" nie ma jeszcze potomków to musi zadbać o usunięcie samego siebie 
// i poinformowanie o tym fakcie "nadzorcy" za pomocą komunikatu.
// case class Pusto(n: Int)
// postaci Pusto(wartość).
// Aktor typu
// class Nadzorca extends Actor { ... }
// czuwa nad całością i reaguje na komunikaty Wstaw, Usuń oraz Pusto i musi zadbać o tworzenie "elementów-korzeni" 
// w sytuacji, gdy dana wartość nie jest w systemie w danym momencie reprezentowana. 
// W tym celu, w swojej "tożsamości" przechowuje zbiór
// def stan(listy: Set[Int]): Receive = { ... }
// Otrzymując komunikat Wstaw(n) taki, że listy.contains(n) "wie", że musi utworzyć "korzeń" dla wartości n.
// Otrzymując komunikat Pusto "nadzorca" musi stosownie zmodyfikować swój stan 
// (tzn. zamienić tożsamość na nową, w której zbiór stan ma uaktualnioną wartość).
// Dla poprawnego działania ważne jest, aby nadzorca, tworząc kolejne korzenie nadawał im nazwy postaci rootNNN, 
// a komunikaty Wstaw oraz Usuń rozsyłał (jedynie) do nich – korzystając z konstrukcji
// context.actorSelection("/user/nadzorca/root*") ! ???
// gdzie nadzorca jest nazwą nadaną nadzorcy w momencie powoływania go do życia.


case class Wstaw2(n: Int)
case class Usuń(n: Int)
case class Pusto(n: Int)

class Element extends Actor {

  def receive: Receive = {
    case Wstaw2(x) => context.become(korzeń(x))
    case _ => ???
  }

  def korzeń(wartość: Int): Receive = {
    case Wstaw2(x) if (x == wartość){
      val n =  context.actorOf(Props[Element])
      n ! Wstaw2(x)
      context.become(zPotomkami(Set(n)))
    }
    case Usuń(x) if (x == wartość){
      
      context.stop(self)
    }
    case _ => ???
  }

  def zPotomkami(wartość: Int, potomkowie: Set[ActorRef]): Receive = {
    ???
  }

}

class Nadzorca extends Actor {
  def receive: Receive = {
    case Wstaw2(x) => {
      val n =  context.actorOf(Props[Element], s"${x}")
      n ! Wstaw2(x)
      context.become(stan(Set(x)))
      }
  }

  def stan(znane: Set[Int]): Receive = {
    case Wstaw2(x) if znane.contains(x)=> {
      context.actorSelection("/user/nadzorca/root*") ! Wstaw2(x)
      }
    case Wstaw2(x) => {
      val n =  context.actorOf(Props[Element], s"root${x}")
      n ! Wstaw2(x)
      context.become(stan(znane + x))
      }
    case Usuń(x) if znane.contains(x)=> {
      context.actorSelection("/user/nadzorca/root*") ! Usuń(x)
      }
    case Pusto(x) => {
      context.become(stan(znane - x))
      }
  }
}

object Zad2 extends App {
  val system = ActorSystem("system")
  val nadzorca = system.actorOf(Props[Nadzorca], "nadzorca")
}
