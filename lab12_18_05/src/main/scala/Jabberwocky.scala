import akka.actor.{ActorSystem, Actor, ActorRef, Props, Terminated, Stash}
import scala.concurrent.duration._


/*

Zadanie 1.
Stwórz symulację bitwy dwóch rodów używając aktorów. Każdy ród posiada Zamek i 500 Łuczników.
Żeby walka była uczciwa pojedynczy Planista (Scheduler) wydaje obu Zamkom rozkaz strzelania,
który rozsyłany jest wśród Łuczników, którzy w danej chwili bronią murów. Zasady walki stanowią:
  * Zamki zaczynają ze 100 Łucznikami, których nazwiemy "aktywnych obrońców";
  * W każdej chwili Zamku może bronić maksymalnie 100 aktywnych obrońców;
  * Scheduler wysyła obu Zamkom rozkaz strzelania co 1 sekundę;
  * Strzały trafiają w Zamek i spadają na jego Łuczników,
    mając szansę na trafienie równą ([liczba_aktywnych_obrońców]/(2 * 100)), np:
      * gdy przeciwnik ma 100 aktywnych Łuczników, nasz strzał ma 50% szansy na trafienie;
      * gdy przeciwnik ma 50 aktywnych Łuczników, nasz strzał ma 25% szansy na trafienie.
  * Obrońca traci życie gdy otrzyma postrzał;
  * Zamek uzupełnia swoich obrońców do momentu wyczerpania wszystkich rezerwowych Łuczników;
  * Żeby być zdolnym do walki Zamek musi mieć przynajmniej jednego aktywnego Łucznika;
  * Gdy wszyscy Łucznicy z danego Zamku zginą, ogłasza on, że przegrał bitwę – kończy to symulację (system.terminate).
Uwagi do rozważenia:
  * To, że strzelanie odbywa się co 1 sekundę nie jest ważne – może to być 0.01 sekundy żeby bitwa trwała krócej.
    Ważne żeby każdy Łucznik miał taką samą szybkość strzału.
  * Możliwe są różne strategie uzupełniania poległych obrońców. Pytanie, czy lepiej częściej uzupełniać Łuczników,
    aby mieć ich jak najwięcej czy trzymać się mniejszej liczby – aby strzały przeciwnika częściej pudłowały?
Uwagi techniczne:
  * Zaimplementuj symulację i wymyśl/przetestuj przynajmniej dwie różne strategie uzupełniania Łuczników.
    Przeprowadzając kilka symulacji, sprawdź która najlepiej się sprawdza.
  * W celu poinformowania Zamku, że jego obrońca ginie, z poziomu Zamku, wykorzystaj metodę context.watch(obrońca).
    Wówczas w przypadku śmierci obrońcy Zamek automatycznie otrzyma komunikat Terminated(obrońca),
    na który powinien sensownie zareagować.
Uwagi "metodologiczne":
  * Na czym polegają Twoje strategie i która z nich działa najlepiej?
  * Możesz skonfrontować swoje strategie ze strategiami opracowanymi przez innych studentów i sprawdzić jaka strategia daje najlepsze wyniki.
Podpowiedź: Do stworzenia rozwiązania użyj szablonu projektu zawartego w pliku lab12.zip.

*/



/*
  W konfiguracji projektu wykorzystana została wtyczka
  sbt-revolver. W związku z tym uruchamiamy program poleceniem

    reStart

  a zatrzymujemy pisząc (mimo przesuwających się komunikatów)

     reStop

  i naciskając klawisz ENTER. Jeśli czynności powyższe
  już wykonywaliśmy to możemy też przywołać poprzednie
  polecenia używając strzałek góra/dół na klawiaturze.
*/

// Przykład wykorzystania Planisty (Scheduler)


// object Jabberwocky {
//   object TickActor {
//     val Tick = "tick"
//   }
//   class TickActor extends Actor {
//     import TickActor._
//     def receive = {
//       case Tick => println("Tick")
//     }
//   }
//   def main(args: Array[String]): Unit = {
//     val system = ActorSystem("system")
//     import system.dispatcher
//     val tickActor = system.actorOf(Props[TickActor](), "defender")
//     val ticker = system.scheduler.scheduleWithFixedDelay(
//       Duration.Zero,
//       50.milliseconds,
//       tickActor,
//       TickActor.Tick
//     )
//     // system.terminate()
//   }
// }
case class Init(x:Int, s:ActorSystem, z:ActorRef)
case object Rekrutuj
case object Kill
object Salwa{
  val Strzela = ("Kill")
}
case class Wrogowie(w: Set[ActorRef])
case object Defeat
class Zamek extends Actor with Stash{
  import Salwa._
  def receive : Receive = {
    case Init(x, system, z) => {
      //val system = ActorSystem("Halo")
      println("Inicjalizacja " + self.path.name)
      val łucznicy = for (i <- 1 to 500) yield system.actorOf(Props[Łucznik], "lucznik_" + i +"_z_zamku_" + x)
      x match{
        case 1 =>{
          val l = łucznicy.toSet.splitAt(100)
          val (obrona, rezerwa) = l
          obrona.map( x => {
            context.watch(x)
            x!Rekrutuj
            })
          rezerwa.map(x => context.watch(x))
          //println(obrona)
          //z!Wrogowie(obrona)
          context.become(wojna(obrona,rezerwa, z, system))
        }
        case 2 =>{
          val l = łucznicy.toSet.splitAt(40)
          val (obrona, rezerwa) = l
          obrona.map( x => {
            context.watch(x)
            x!Rekrutuj
            })
          rezerwa.map(x => context.watch(x))
          //println(obrona)
          //z!Wrogowie(obrona)
          context.become(wojna(obrona,rezerwa, z, system))
        }
      }
      
    }
    //case _ => stash()
  }
  // def wypowiedzenieWojny(obrona:Set[ActorRef], rezerwa:Set[ActorRef], z:ActorRef) : Receive ={
  //   case Wrogowie(w) => {
  //     println("WYPOWIEDZENIE WOJNY " + self.path.name)
  //     context.become(wojna(obrona,rezerwa, w, z))}
  //   case _ => stash()
  // }
  def wojna(obrona:Set[ActorRef], rezerwa:Set[ActorRef], z:ActorRef, s:ActorSystem) : Receive ={
    //case "open" => unstashAll()
    case Strzela => {
      //println(Strzela)
      z ! Wrogowie(obrona)
    }
    case Wrogowie(wrogowie) => {
      val l = wrogowie.size
      val r = scala.util.Random
      wrogowie.map(w =>{
        val i = r.nextFloat
        val szansa = (l/200.0)
        //println((i, szansa, l))
        if (i<= szansa) {
          println("Zabijam " + w.path.name)
          w ! Kill
          }// else { println (w.path.name + " Przezyl")}
      //context.become(wojna(obrona,rezerwa, x, z))
      })
    }
    case Terminated(a) => {
      //println(a.path.name + "Umarl XXXXXXXXX")
      if (obrona == Set() && rezerwa == Set()){
          println(self.path.name + "Przegrana")
          z ! Defeat
          context.stop(self)
      } 
      else if (rezerwa == Set()){
        //println("Skonczyla sie rezerwa" )
        val o = obrona - a
        //println(o)
        if (o == Set()){
          println(self.path.name + "Przegrana")
          z ! Defeat
          context.stop(self)
        } else { context.become(wojna(o,rezerwa, z, s)) }
      } 
      else {
        val r = rezerwa.head
        val o = obrona - a + r
        //println(r.path.name + " REKRUTACJAAA")
        r ! Rekrutuj
        context.become(wojna(o,rezerwa - r, z, s))
      }
    }
    case Defeat => {
      println(self.path.name + "Wygrana")
      context.stop(self)
      s.terminate()
    }
  }
}
class Łucznik extends Actor{
  def receive : Receive = {
    case Rekrutuj => {
      //println(self.path.name)
      context.become(obrona)}
  }
  def obrona : Receive = {
    case Kill => {
      //println(self.path.name + " Umarlem")
      context.stop(self)
      }
  }
}

object Main {
  def main(args: Array[String]): Unit = {
    val system = ActorSystem("HaloAkka")
    val zamek1 = system.actorOf(Props[Zamek](), s"zamek1")
    val zamek2 = system.actorOf(Props[Zamek](), s"zamek2")
    zamek1 ! Init(1, system, zamek2)
    zamek2 ! Init(2, system, zamek1)
    // zamek1 ! Strzela
    // zamek2 ! Strzela
    // zamek1 ! Strzela
    // zamek2 ! Strzela
    import system.dispatcher
    val planista = system.scheduler.scheduleWithFixedDelay(
      Duration.Zero,
      60.milliseconds,
      zamek1,
      Salwa.Strzela
    )
    val planista2 = system.scheduler.scheduleWithFixedDelay(
      Duration.Zero,
      60.milliseconds,
      zamek2,
      Salwa.Strzela
    )
  }
}