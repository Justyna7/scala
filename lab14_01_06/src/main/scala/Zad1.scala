/*
Zadanie 1. Wykorzystując "zalążek" znajdujący się w pliku lab14.zip zaimplementuj "symulator konkursu", 
który rozgrywany jest w dwóch turach – eliminacyjnej i finałowej. W eliminacjach bierze udział 50 
(losowo wygenerowanych) zawodników, z których 20. z najlepszymi wynikami przechodzi następnie do rundy finałowej.
System aktorów składa się z organizatora, który steruje całością, wylosowuje zawodników, 
inicjuje rozgrywanie obu rund, agreguje ich wyniki oraz oblicza i prezentuje klasyfikację. 
Jest on reprezentantem klasy Organizator.
Oprócz organizatora, w symulatorze występują (tworzeni przez niego) aktorzy klasy Runda. 
Ich zadaniem jest przeprowadzenie pojedynczych rund konkursu. 
Odbywa się to poprzez sekwencyjne wykonywanie "prób" przez kolejnych zawodników. 
Runda powinna na bieżąco przekazywać "surowe" wyniki tych prób do organizatora.
Kiedy runda konkursu dobiegnie końca, a dane na temat jej wyników znajdą się u organizatora, 
ten ostatni kończy działanie aktora rundy (wysyłając do niego odpowiedni komunikat). 
Organizator oblicza wyniki i wyłania na ich podstawie 20. uczestników rundy finałowej. 
Następnie tworzy kolejnego aktora typu Runda i przekazuje mu finalistów (za pomocą komunikatu Runda.Start(…)).
Klasyfikacja rundy ustalana jest na podstawie porządku opisanego w pliku Ocena.scala 
(w podkatalogu zad1 głównego katalogu aplikacji). 
W przypadku rundy finałowej, ostateczna ocena powstaje przez zsumowanie odpowiednich not "po współrzędnych", np.
ocenaKwalifikacyjna == Ocena(10, 12, 17)
ocenaFinałowa == Ocena( 8, 17, 10)

ocenKońcowa == Ocena(18, 29, 27)
Organizator, reagując na komunikat Organizator.Wyniki wyświetla na konsoli aktualną klasyfikację. 
Wyświetlając ją zadbaj, aby poprawnie traktowane były sytuacje remisowe – np.
1. John Williams – 18-29-27 = 74
2. Bill Wayman – 17-26-21 = 64
2. Ronald Curtis – 17-26-21 = 64
4. Thomas Frank – 16-26-22 = 64
...
Zwróć uwagę, że "Thomas Frank" zajął miejsce 4. a nie 3. Symulator sterowany jest poprzez "interfejs użytkownika", 
który bezpośrednio komunikuje się z organizatorem. Szkielet kodu interfejsu znajduje się w pliku Zad1.scala.

Podpowiedź: Do generowania danych typu Osoba wykorzystaj przygotowaną metodę Utl.osoba(), 
a do generowania ocen – metodę Utl.ocena() (zwraca ona wartość typu Option[Ocena]).
Autor zadania: dr W. Pawłowski
*/



// „Interfejs użytkownika” wymaga pewnych dodatkowych elementów:

import scala.concurrent.ExecutionContext
import scala.util.control.Breaks._
import scala.io.StdIn

import akka.actor.{ActorSystem, Props}
import zad1._

object Zad1 extends App {

  val system = ActorSystem("system")
  val organizator = system.actorOf(Props[Organizator](), "organizator")


  // Interfejs „organizatora”:
  implicit val ec: ExecutionContext = ExecutionContext.global

  breakable {
    while (true) {
      StdIn.readLine("polecenie: ") match {
        case "start" => organizator ! Organizator.Start
          // początek zawodów
        case "eliminacje" =>{
          println("Rozpoczynamy eliminacje")
          organizator ! Organizator.Runda
          // polecenie rozegrania rundy eliminacyjnej
        }
        case "final" =>{
          println("Rozpoczynamy finały")
          organizator ! Organizator.Runda
          // polecenie rozegrania rundy finałowej
          // wymaga zamknięcia Rundy eliminacyjnej i utworzenie
          // Rundy finałowej, zawierającej najlepszych 20.
          // zawodników z Rundy eliminacyjnej
        }
        
        case "wyniki" => organizator ! Organizator.Wyniki
          // żądanie rankingów (lub rankingu finałowego)
        case "koniec" =>{
          organizator ! Organizator.Stop
          break()
        }
        case _ =>
      }
    }
  }

}
