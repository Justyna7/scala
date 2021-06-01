package zad1

import org.scalacheck._
import uk.gov.hmrc.smartstub._

// Generator osób i ocen
object Utl {
  import Osoba._
  import scala.util.Random
  val rand = new Random
  val próbaUdana = 0.05

  def osoba(): Osoba = {
    val imie = Gen.forename().sample.get
    val nazwisko = Gen.surname.sample.get
    Osoba(imie, nazwisko)
  }

  def ocena(): Option[Ocena] = {
    if (rand.nextDouble() > próbaUdana) {
      val nota1 = rand.nextInt(21)
      val nota2 = rand.nextInt(21)
      val nota3 = rand.nextInt(21)
      Some(Ocena(nota1, nota2, nota3))
    } else {
      None
    }
  }

}
