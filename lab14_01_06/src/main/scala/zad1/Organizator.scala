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
  def receive: Receive = {
    case Start => {
      // tworzenie 50. osób, opdowiadających im Zawodników
      // oraz Grupy eliminacyjnej
      val o = for (i <- 1 to 50) yield context.actorOf( new Props[Zawodnik(Utl.osoba())], "zawodnik" + i.toString)
    }
      
      // Obsługa pozostałych komunikatów

    case Stop =>
      println("kończymy zawody...")
      context.system.terminate()
  }
}
