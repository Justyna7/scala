package Lab10
import akka.actor.{ActorSystem, Actor, ActorRef, Props, ActorSelection}

object Zad3 {
  // Zadanie 3. Zmodyfikuj/uogólnij rozwiązanie z zadania 2 tak,
  // aby rozgrywka mogła odbywać się "po okręgu" składającym się z zadanej liczby aktorów.
  case object Piłeczka
  case class Graj(a: ActorSelection, p: Boolean, i: Int)
  class Gracz extends Actor { 
    def receive: Receive = {
      case Graj(a, p, i) =>{
        println(""+ self.path.name + " rozpoczynam gre")
        context.become(graj(a, p, i))
      }
    }
    def graj(a:ActorSelection, ping: Boolean, ile: Int): Receive = {
      case Piłeczka => {
        ping match{
          case true => println(self.path.name + " ping")
          case false => println(self.path.name + " pong")
        }
        if (ile%2 == 1) {context.become(graj(a, !ping, ile))}
        a ! Piłeczka
      }
    }
  }
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("sys")
    val n = 7
    val actor = for (i <- 1 to n) yield system.actorOf(Props[Gracz], s"gracz$i")
    //println(actor.toList.map(x => x.path.name))
    actor.toList.map(a => a.path.name match{
      case s"gracz$i" if (i.toInt == n && i.toInt%2 == 0) =>
        a ! Graj(system.actorSelection("/user/gracz1"), false, n)
      case s"gracz$i" if (i.toInt == n) => 
        a ! Graj(system.actorSelection("/user/gracz1"), true, n)
      case s"gracz$i" if (i.toInt%2 == 0) => 
        a ! Graj(system.actorSelection("/user/gracz"+(i.toInt+1).toString), false, n)
      case s"gracz$i" => 
        a ! Graj(system.actorSelection("/user/gracz"+(i.toInt+1).toString), true, n)
    })
    system.actorSelection("/user/gracz1") ! Piłeczka
  }

}
