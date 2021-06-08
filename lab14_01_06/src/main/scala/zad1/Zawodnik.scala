package zad1

import akka.actor.Actor

object Zawodnik {
  case object Próba
  //case object Imie
  // polecenie wykonania próby (kończy się zwróceniem Wyniku,
  // za pomocą komunikatu Grupa.Wynik)
}

class Zawodnik extends Actor {
  import Zawodnik._
  import Utl._
  import Grupa._
  def receive: Receive = {
    case Próba => {
      val n = Utl.ocena()
      sender ! Grupa.Wynik(n)
    }
    case msg => println(msg)
  }
}
