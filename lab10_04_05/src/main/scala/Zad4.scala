package Lab10
import akka.actor.{ActorSystem, Actor, ActorRef, Props}

object Zad4 {
    // Zadanie 4. Dokończ implementację "sita Eratostenesa", czyli (opartej o wykorzystanie aktorów)
    // implementacji klasycznego algorytmu znajdowania liczb pierwszych w przedziale [2..N],
    // dla zadanej wartości N. Zalążek rozwiązania pojawił się na pierwszym wykładzie z Akki.
    import akka.actor._
    case class Start(max: Int)
    case class Number(n: Int)

    class Nadzorca extends Actor {
        def receive: Receive = {
            case Start(m) =>
                if (m >= 2) {
                    val pracownik = context.actorOf(Props[Pracownik]())
                    // for (n <- 2 to m + 1) { pracownik ! Number(n) }
                }
        }
    }

    class Pracownik extends Actor {
        def receive: Receive = {
            // musimy reagować na napływające komunikaty
            // i stosownie do nich działać ;)
            case msg =>
                println(msg)
        }
    }

    def main(args: Array[String]): Unit = {
        val system = ActorSystem("Eratostenes")
        //....
    }
}
