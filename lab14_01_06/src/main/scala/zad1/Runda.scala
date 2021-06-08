package zad1

import akka.actor.{Actor, ActorRef, ActorSelection}

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
  import Organizator._
  import Grupa._
  import Zawodnik._
  def receive: Receive = {
    case Grupa.Runda => {
      zawodnicy.foreach( x => x ! Zawodnik.Próba)
      context.become(tura(List()))
      }
    case msg => println(msg)
  }
  def tura(wyniki: List[(ActorRef, Option[Ocena])]) : Receive = {
    case Wynik(w) => {
      //println(sender.path.name, w)
      val n = ((sender, w)::wyniki)
        .sortBy(n => (n._2 match{
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
      if (n.length == zawodnicy.length){
        val organizator = context.actorSelection("/user/organizator")
        organizator ! Organizator.Wyniki(
          wyniki.filter( n => (n._2 match{
            case None => false
            case _ => true }))
          .toMap)
      }else{
        context.become(tura(n))
      }
      
    }
    case Grupa.Wyniki => {
        println(wyniki.filter( n => (n._2 match{
            case None => false
            case _ => true })).map( x => (wyniki.indexOf(x)+1, x._1.path.name, x._2, x._2 match{//
          case Some(Ocena(a, b, c)) => a+b+c
          case None => 0
        }))
        
      )
    }
  }
}
