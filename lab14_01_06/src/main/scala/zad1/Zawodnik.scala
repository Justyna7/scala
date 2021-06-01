package zad1

import akka.actor.Actor

object Zawodnik {
  case object Próba
  case object Imie
  // polecenie wykonania próby (kończy się zwróceniem Wyniku,
  // za pomocą komunikatu Grupa.Wynik)
}

class Zawodnik(osoba: Osoba) extends Actor {
  import Zawodnik._
  import Utl._
  def receive: Receive = {
    case msg => println(msg)
  }
}
