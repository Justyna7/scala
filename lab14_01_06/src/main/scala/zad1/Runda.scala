package zad1

import akka.actor.{Actor, ActorRef}

object Grupa {
  case object Runda
  // Zawodnicy mają wykonać swoje próby – Grupa
  // kolejno (sekwencyjnie) informuje zawodników
  // o konieczności wykonania próby i „oczekuje”
  // na ich wynik (typu Option[Ocena])
  case object Wyniki
  // Polecenie zwrócenia aktualnego rankingu Grupy
  // Oczywiście klasyfikowani są jedynie Zawodnicy,
  // którzy pomyślnie ukończyli swoją próbę
  case class Wynik(ocena: Option[Ocena])
  // informacja o wyniku Zawodnika (wysyłana przez Zawodnika do Grupy)
  // Jeśli zawodnik nie ukończy próby zwracana jest wartość None
  case object Koniec
  // Grupa kończy rywalizację
}
class Grupa(zawodnicy: List[ActorRef]) extends Actor {
  def receive: Receive = {
    case msg => println(msg)
  }
}
