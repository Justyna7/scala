package Lab10
import akka.actor.{ActorSystem, Actor, ActorRef, Props}

object Zad1 {
  // Zadanie 1. Zdefiniuj klasę typu: Pracownik
  // class Pracownik extends Actor { ... }
  // który będzie przyjmował komunikaty typu
  // case class Wynik(liczba1: Double, liczba2: Double)
  // który wypisze sumę dwóch liczb. Dodatkowo powinien móc przyjąć komunikat typu
  // case object Zmien
  // zmieniający stan na taki w którym po otrzymaniu komunikatu
  // case class Wynik(liczba1: Double, liczba2: Double)
  // zamiast sumy wyświetlony zostanie iloczyn.
  // Dodatkowo powinien móc przyjąć komunikat Zmien umożliwiający powrót do wcześniejszego stanu.


  case class Wynik(liczba1: Double, liczba2: Double)
  case class Aktor (actor: ActorRef)
  case class Rozwiazanie(wynik: Double)
  case object Zmien

  class Pracownik extends Actor { 
    // def receive: Receive = { // każdy aktor musi mieć tą metodę
    //   case Wynik(x,y) => println(x+y)
    //   case Zmien => context.become(wykonaj)
    // } 
    // def wykonaj: Receive ={
    //   case Wynik(a, b) => println(a*b)
    //   case Zmien => context.become(receive)
    // }
    
    // Wersja 2:
    def receive: Receive = { // każdy aktor musi mieć tą metodę
      case Wynik(x,y) => println(x+y)
      case Zmien => context.become(wykonaj(1))
      case Aktor(a) => a! Wynik(1,2) // a - odwołanie do aktora; odwołanie do siebie => self
      case Rozwiazanie(a) => println(a)
    } 
    def wykonaj(x:Int): Receive ={
      case Wynik(a, b) => if(x == 1) println(a*b) else println(a+b)
      case Zmien => if (x==1) context.become(wykonaj(0)) else context.become(wykonaj(1))
    }
  }

  def main(args: Array[String]): Unit = {
    val system = ActorSystem("sys")
    val pracownik = system.actorOf(Props[Pracownik], "pracownik")
    val actor = for (i <- 1 to 10) yield system.actorOf(Props[Pracownik], s"pracownik$i")
    println(actor.toList)
    actor.toList.foreach(a => a!Aktor(pracownik))

    // pracownik ! Wynik(2,3)
    // pracownik ! Zmien
    // pracownik ! Wynik(2,3)
    // pracownik ! Zmien
    // pracownik ! Wynik(2,3)
  }

}
