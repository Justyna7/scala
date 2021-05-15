package Lab10
import akka.actor.{ActorSystem, Actor, ActorRef, Props}

object Zad2 {
  // Zadanie 2. Zdefiniuj klasę
  // class Gracz extends Actor { ... }
  // tak, aby po utworzeniu trzech aktorów typu Gracz mogli oni grać w "ping-ponga" w trójkącie.
  // Jako "piłeczki" użyj obiektu
  // case object Piłeczka
  case object Piłeczka
  case class Graj(a:ActorRef, p:Boolean)
  class Gracz extends Actor { 
    def receive: Receive = {
      case Graj(a, p) =>{
        println(""+ self.path.name + " rozpoczynam gre")
        context.become(graj(a, p))
      }
    }
    def graj(a:ActorRef, ping: Boolean): Receive = {
      //case Graj => a ! Piłeczka
      case Piłeczka => {
        ping match{
          case true => println(self.path.name + " ping")
          case false => println(self.path.name + " pong")
        }
        context.become(graj(a, !ping))
        a ! Piłeczka
      }
    }
   }
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("sys")
    val gracz1 = system.actorOf(Props[Gracz], "gracz1")
    val gracz2 = system.actorOf(Props[Gracz], "gracz2")
    val gracz3 = system.actorOf(Props[Gracz], "gracz3")
    gracz1 ! Graj(gracz2, true)
    gracz2 ! Graj(gracz3, false)
    gracz3 ! Graj(gracz1, true)
    gracz1 ! Piłeczka
  }
  
}
