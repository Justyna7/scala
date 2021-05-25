import akka.actor._

// Zadanie 1. Zdefiniuj klasę
// class Węzeł extends Actor { ... }
// tak, aby aktorzy przez nią określeni mogli posłużyć do budowy "binarnych drzew poszukiwań". 
// Powinni oni reagować na dwa typy komunikatów:
// case class Wstaw(n: Int)
// case class Znajdź(n: Int)
// Każdy Węzeł po utworzeniu, korzystając z "tożsamości"
// def liść(wartość: Int): Receive = { ... }
// przechowuje liczbę, którą otrzymał w pierwszym komunikacie Wstaw. Kolejne komunikaty tej postaci powinny spowodować, 
// że węzeł (w zależności od ich zawartości) zmieni swoją tożsamość na jedną z:
// def zLewymPoddrzewem(lewe: ActorRef, wartość: Int): Receive = { ... }
// def zPrawymPoddrzewem(wartość: Int, prawe: ActorRef): Receive = { ... }
// def zPoddrzewami(lewe: ActorRef, wartość: Int, prawe: ActorRef): Receive = { ... }
// Wybór tożsamości zależy od tego czy otrzymana liczba n jest mniejsza (wtedy przechodzimy do zLewymPoddrzewem) 
// lub większa (przechodzimy do zPrawymPoddrzewem) od wartość. Jeśli n == wartość to taki komunikat Węzeł pomija.
// W ten sposób, wraz z kolejnymi komunikatami Wstaw powstaje binarne drzewo złożone z aktorów, które ma własność: 
// „dla każdego węzła wartości węzłów w jego lewym poddrzewie są mniejsze, a w prawym – większe od wartość.
// Komunikat Znajdź(n) powinien "spłynąć" po drzewie (od jego korzenia w dół), 
// aż do węzła, który może jednoznacznie stwierdzić, że zawiera on jego wartość, bądź, że w drzewie brak liczby n. 
// Fakt znalezienia (bądź nie) liczby n aktor powinien zasygnalizować komunikatem na konsoli.

case class Wstaw(n: Int)
case class Znajdź(n: Int)

class Węzeł extends Actor {

  def liść(wartość: Int): Receive = {
    case Wstaw(x) => {
      if (x < wartość){
        val n = context.actorOf(Props[Węzeł], "L")
        n ! Wstaw(x)
        context.become(zLewymPoddrzewem(n, wartość))
      } else if (x > wartość){
        val n = context.actorOf(Props[Węzeł], "P")
        n ! Wstaw(x)
        context.become(zPrawymPoddrzewem(wartość, n))
      }
    }
    case Znajdź(x) => if (x == wartość){
      println(s"Znalazlem ${self.path} ${x}")
    }else{
      println("Nie ma")
    }
  }

  def zLewymPoddrzewem(lewe: ActorRef, wartość: Int): Receive = {
    case Wstaw(x) =>{
      if (x < wartość){
        lewe ! Wstaw(x)
      } else if (x > wartość){
        val n = context.actorOf(Props[Węzeł], "P")
        n ! Wstaw(x)
        context.become(zPoddrzewami(lewe, wartość, n))
      }
    }
    case Znajdź(x) =>{
      if (x < wartość){
        lewe ! Znajdź(x)
      } else if (x > wartość){
        println("Nie ma")
      } else { println(s"Znalazlem ${self.path} ${x}")}
    }
  }

  def zPrawymPoddrzewem(wartość: Int, prawe: ActorRef): Receive = {
    case Wstaw(x) =>{
      if (x > wartość){
        prawe ! Wstaw(x)
      } else if (x < wartość){
        val n = context.actorOf(Props[Węzeł], "L")
        n ! Wstaw(x)
        context.become(zPoddrzewami(n, wartość, prawe))
      }
    }
    case Znajdź(x) =>{
      if (x > wartość){
        prawe ! Znajdź(x)
      } else if (x < wartość){
        println("Nie ma")
      } else { println(s"Znalazlem ${self.path} ${x}")}
    }
  }

  def zPoddrzewami(lewe: ActorRef, wartość: Int, prawe: ActorRef): Receive = {
    case Wstaw(x) =>{
      if (x < wartość){
        lewe ! Wstaw(x)
      } else if (x > wartość){
        prawe ! Wstaw(x)
      }
    }
    case Znajdź(x) =>{
      if (x < wartość){
        lewe ! Znajdź(x)
      } else if (x > wartość){
        prawe ! Znajdź(x)
      } else { println(s"Znalazlem ${self.path} ${x}")}
    }
  }

  def receive: Receive = {
    case Wstaw(x) => context.become(liść(x))
    case Znajdź(x) => println("Taka wartość jeszcze nie istnieje")
  }
}

object Zad1 extends App {
  val system = ActorSystem("system")
  val root = system.actorOf(Props[Węzeł], s"root")
  root ! Znajdź(5)
  root ! Wstaw(3)
  root ! Znajdź(3)
  root ! Wstaw(8)
  root ! Wstaw(12)
  root ! Wstaw(10)
  root ! Znajdź(10)
}
