package lab11

// Zadanie 1. Używając aktorów zaimplementuj "rozproszony licznik różnych słów".
// Powinien on składać się z aktora głównego, typu Nadzorca
// class Nadzorca extends Actor { ... }
// oraz dynamicznie określanej (w momencie inicjowania działania nadzorcy) liczby aktorów "roboczych", typu Pracownik
// class Pracownik extends Actor { ... }
// Po uruchomieniu, Nadzorca jest w stanie przyjąć jedynie komunikat inicjalizacyjny postaci
// case class Init(liczbaPracownikow: Int)
// w którego efekcie powinien utworzyć zadaną liczbę aktorów typu Pracownik i przejść do stanu,
// w którym jest w stanie przyjmować "zlecenia" nadsyłane za pomocą komunikatów
// case class Zlecenie(tekst: List[String])
// Poszczególne napisy z listy tekst powinny być następnie przekazane do "obróbki" pracownikom,
// zgodnie z zasadą "w kółko Macieju" (ang. "round Robin"). Służyć do tego powinny komunikaty
// case class Wykonaj( /* argumenty */ )
// Pracownicy zwracają informację o liczbie znalezionych różnych słów 
// (np. "Ulica" i "ulica" to identyczne słowa). do nadzorcy, używając komunikatów
// case class Wynik( /* argumenty */ )
// Nadzorca sumuje napływające wyniki i po otrzymaniu wszystkich odpowiedzi od pracowników wypisuje
// na konsoli wynik i wraca do stanu oczekiwania na kolejne zlecenie.

// Podpowiedź 1: Do stworzenia rozwiązania użyj szablonu projektu zawartego w pliku lab11.zip.
// Podpowiedź 2:
// Do przetestowania działania "licznika słów" wykorzystaj dane zwracane przez funkcję
// def dane(): List[String] = {
//  scala.io.Source.fromResource("ogniem_i_mieczem.txt")
//  .getLines
//  .toList
// }
// Plik ogniem_i_mieczem.txt znajduje się już w odpowiednim katalogu szablonu rozwiązania.


import akka.actor.{ActorSystem, Actor, ActorRef, Props}


case class Init(liczbaPracownikow: Int)
case class Zlecenie(tekst: List[String])
case class Wykonaj( s: String)
case class Wynik(x: List[(String,Int)] )

// class MyActor extends Actor {
//   def receive: Receive = {
//     case msg => println(s"Odebrałem wiadomość: $msg")
//   }
// }
class Nadzorca extends Actor { 
  def receive: Receive = {
    case Init(n) => {
      println("Nadzorca zainicjowany")
      val pracownicy = for (i <- 1 to n) yield Main.system.actorOf(Props[Pracownik], s"pracownik$i")
      //println(pracownicy.toList)
      //actor.toList.foreach(a => a!Aktor(pracownik))
      println("Nadzorca czeka na zlecenie")
      context.become(przyjmijZlecenie(pracownicy.toList))
      }
  }
  def przyjmijZlecenie(p: List[ActorRef]): Receive ={
    case Zlecenie(txt) => {
      println("Otrzymano zlecenie")
      val t = txt.filterNot(x=> x== "")
      t.grouped(p.length).toList.map(a => a.foreach(x => p(a.indexOf(x)) ! Wykonaj(x)))
      context.become(agreguj(List(), t.length - 1, p))
    }
  }
  def agreguj(l: List[(String,Int)], i: Int, p: List[ActorRef]): Receive = {
    case Wynik(x) => {
      val n = l.concat(x).groupBy(_._1).map(x => x._2.foldLeft(("", 0.toInt))((a,b) => (b._1, a._2 + b._2))).toList
      //val s = l.concat(x)
      if (i == 0) { 
        println("Mam wynik: " + n)
        println("Nadzorca czeka na zlecenie")
        context.become(przyjmijZlecenie(p)) 
      }
      else { context.become(agreguj(n, i-1, p)) }
    }
  }
 }
class Pracownik extends Actor { 
  def receive: Receive = {
    case Wykonaj(s) => {
      val x = s.split(" ").toList.map(x => x.filter(x => x.isLetter).toLowerCase).filterNot(x=> x== "")
      val n = x.map(a => (a,x.count(_ == a))).foldLeft(List[(String,Int)]())((a,b) => if(a.contains(b)) a else b::a)
      //println(self.path.name + " " + n)
      sender ! Wynik(n)
      }
  }
 }

object Main {
  val system = ActorSystem("HaloAkka")
  def dane(): List[String] = {
    scala.io.Source.fromResource("ogniem_i_mieczem.txt").getLines.toList
  }

  def main(args: Array[String]): Unit = {
    
      // val leonardo = system.actorOf(Props[MyActor], "leonardo")
      // leonardo ! "Dostałeś Oskara!"
    val nadzorca = system.actorOf(Props[Nadzorca], s"nadzorca")
      nadzorca ! Init(3)
      nadzorca ! Zlecenie(dane)
      //nadzorca ! Zlecenie(dane)
  }

}
