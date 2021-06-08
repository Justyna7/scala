package zad1

import akka.actor.{Actor, ActorRef, Props}

object Organizator {
  case object Start
  // rozpoczynamy zawody – losujemy 50 osób, tworzymy z nich zawodników
  // i grupę eliminacyjną
  case object Runda
  // polecenie rozegrania rundy (kwalifikacyjnej bądź finałowej) –  wysyłamy Grupa.Runda
  // do aktualnej grupy
  case object Wyniki
  // polecenie wyświetlenia klasyfikacji dla aktualnej grupy
  case class Wyniki(w: Map[ActorRef, Option[Ocena]])
  // wyniki zwracane przez Grupę
  case object Stop
  // kończymy działanie
}

class Organizator extends Actor {
  // importujemy komunikaty na które ma reagować Organizator
  import Organizator._
  import Utl._
  import Osoba._
  import Zawodnik._
  // import Runda._
  // import Runda.Grupa._
  import Grupa._
  def receive: Receive = {
    case Start => {
      // tworzenie 50. osób, opdowiadających im ZawodnikówS
      // oraz Grupy eliminacyjnej
      println("ZACZYNAMY")
      println(self)
      val o = for (i <- 1 to 50) yield {
        val x = Utl.osoba()
        context.system.actorOf(Props[Zawodnik](), x.imie + x.nazwisko.filterNot(x => x ==' ') + i)
      }
      val n = o.toList//.map(x => x.path.name)
      val e = context.system.actorOf( Props( classOf[Grupa],n), s"eliminacje") // nie działa
      println(e)
      context.become(eliminacje(e))
    }
      // Obsługa pozostałych komunikatów
    
    case Stop =>
      println("kończymy zawody...")
      context.system.terminate()
  }
  def eliminacje(grupa: ActorRef):Receive = {
    case Organizator.Runda => grupa ! Grupa.Runda
    case Organizator.Wyniki => grupa ! Grupa.Wyniki
    case Organizator.Wyniki(w) => {
      println("Wyniki rundy eliminacyjnej:")
      grupa ! Grupa.Wyniki
      val e = context.system.actorOf( Props( classOf[Grupa],w.toList.take(20).map(x => x._1)), s"final")
      println(e)
      context.become(finaly(e, w))
    }
    case Stop =>
      println("kończymy zawody...")
      context.system.terminate()
  }
  def finaly(grupa: ActorRef, wyniki_elim: Map[ActorRef, Option[Ocena]]):Receive = {
    case Organizator.Runda => grupa ! Grupa.Runda
    case Organizator.Wyniki => grupa ! Grupa.Wyniki
    case Organizator.Wyniki(w) => {
      println("Wyniki rundy finałowej:")
      grupa ! Grupa.Wyniki
      val wyniki = wyniki_elim.keySet.intersect(w.keySet).map(x => (x,(wyniki_elim(x), w(x)))).toList.map(
        x => (x._1, x._2 match{
          case (Some(Ocena(a,b,c)), Some(Ocena(d,e,f))) => Some(Ocena(a+d,b+e,c+f))
        })
      )
      //println(wyniki)
      context.become(wynik_koncowy(wyniki))
    }
    case Stop =>
      println("kończymy zawody...")
      context.system.terminate()
  }
  def wynik_koncowy(wyniki: List[(ActorRef, Option[Ocena])]):Receive = {
    case Organizator.Wyniki => {
      val a = wyniki.sortBy(n => (n._2 match{
          case Some(Ocena(a, b, c)) => c
          case None => 0}))
        .sortBy(n => (n._2 match{
          case Some(Ocena(a, b, c)) => b
          case None => 0}))
        .sortBy(n => (n._2 match{
          case Some(Ocena(a, b, c)) => a
          case None => 0}))
        .sortBy(n => (n._2 match{
          case Some(Ocena(a, b, c)) => a+b+c
          case None => 0
        })).reverse
      val b = a.map( x => (a.indexOf(x)+1, x._1.path.name, x._2, x._2 match{//
          case Some(Ocena(a, b, c)) => a+b+c
          case None => 0
        }))
      println("Wyniki końcowe")
      println(b)}
    case Stop =>{
      println("kończymy zawody...")
      context.system.terminate()
  }
  }
  
}
