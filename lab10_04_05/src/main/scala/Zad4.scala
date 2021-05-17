package Lab10
import akka.actor.{ActorSystem, Actor, ActorRef, Props}

object Zad4 {
    // Zadanie 4. Dokończ implementację "sita Eratostenesa", czyli (opartej o wykorzystanie aktorów)
    // implementacji klasycznego algorytmu znajdowania liczb pierwszych w przedziale [2..N],
    // dla zadanej wartości N. Zalążek rozwiązania pojawił się na pierwszym wykładzie z Akki.
    import akka.actor._
    case class Start(max: Int)
    case class Number(n: Int)
    case class Lista(m: Int)
    case object Wynik
    @annotation.tailrec
    def lista(x:Int, y:List[Int] = List()): List[Int] = x match{
        case 2 => 2::y
        case a => lista(x-1, a::y)
    }
    class Nadzorca extends Actor {
        def receive: Receive = {
            case Start(m) =>
                if (m >= 2) {
                    val pracownik = context.actorOf(Props[Pracownik]())
                    pracownik ! Lista(m)
                    for (n <- 2 to m+1) { pracownik ! Number(n) }
                    pracownik ! Wynik
                }
        }
    }

    class Pracownik extends Actor {
        def receive: Receive = {
            case Lista(m) => {
                val l = lista(m)
                context.become(przerob(l))
                }
            // musimy reagować na napływające komunikaty
            // i stosownie do nich działać ;)
            // case msg =>
            //     println(msg)
        }
        def przerob(l: List[Int]): Receive = {
            case Number(x) => {
                //println(l)
                if (x > l.max) {context.become(w(l))} 
                else if (l.contains(x)) {
                    context.become(przerob(l.filterNot(a => a%x == 0 && a != x)))
                    } 
            }
        }
        def w(l: List[Int]): Receive = {
            case Wynik => {
                println(l)
            }
            case _ => l
        }
    }

    def main(args: Array[String]): Unit = {
        val system = ActorSystem("Eratostenes")
        val nadzorca = system.actorOf(Props[Nadzorca], "nadzorca")
        nadzorca ! Start(10000)
    }
}
