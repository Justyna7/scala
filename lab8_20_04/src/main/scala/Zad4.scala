object Zad4 {

  case class Województwo(nazwa: String, min: Int)
  // max ID gminy z województwa w: w.min + 19999
  case class Wynik(
    ID: Int,
    KOALICJA_EUROPEJSKA: Int,
    LEWICA_RAZEM: Int,
    POLEXIT: Int,
    JEDNOŚĆ_NARODU: Int,
    PIS: Int,
    EUROPA_CHRISTI: Int,
    WIOSNA: Int,
    KONFEDERACJA: Int,
    KUKIZ15: Int,
    POLSKA_FAIR_PLAY: Int
  )

  def main(args: Array[String]): Unit = {
    val województwa = List(
      Województwo("dolnośląskie",20000),
      Województwo("kujawsko-pomorskie",40000),
      Województwo("lubelskie",60000),
      Województwo("lubuskie",80000),
      Województwo("łódzkie",100000),
      Województwo("małopolskie",120000),
      Województwo("mazowieckie",140000),
      Województwo("opolskie",160000),
      Województwo("podkarpackie",180000),
      Województwo("podlaskie",200000),
      Województwo("pomorskie",220000),
      Województwo("śląskie",240000),
      Województwo("świętokrzyskie",260000),
      Województwo("warmińsko-mazurskie",280000),
      Województwo("wielkopolskie",300000),
      Województwo("zachodniopomorskie",320000)
    )
// Zadanie 4.
// Używając kolekcji i operacji na nich oraz danych z wyników wyborów zawartych w pliku wybory.csv znajdź województwo,
// w którym różnica procentowa głosów oddanych na Koalicję Obywatelską oraz na PiS była minimalna.
// Otrzymane wyniki wyświetl na konsoli – zarówno dane województw(a), jak i wartości procentowe.
// Zauważ, że elementy o wartości "minimalnej/maksymalnej" nie muszą być unikatowe.
// Do reprezentowania danych o województwach i gminach użyj zdefiniowanych w pliku
// Zad4.scala "klas wzorcowych" (case class) Województwo i Wynik.

    val wyniki = io.Source
      .fromResource("wyniki.csv")
      .getLines
      .toList
      .map(l => {
         l.split(",").toList.map(_.toInt) match {
          case List(a,b,c,d,e,f,g,h,i,j,k) => Wynik(a,b,c,d,e,f,g,h,i,j,k)
        }
       })
    val woj = wyniki.groupBy(x=> x.ID/10000)
    .map(x=> (x._1*10000, x._2.foldLeft(Wynik(0,0,0,0,0,0,0,0,0,0,0))
      ((a,b)=> Wynik(a.ID, a.KOALICJA_EUROPEJSKA + b.KOALICJA_EUROPEJSKA,
        a.LEWICA_RAZEM + b.LEWICA_RAZEM, a.POLEXIT+b.POLEXIT,
        a.JEDNOŚĆ_NARODU+ b.JEDNOŚĆ_NARODU, a.PIS + b.PIS,
        a.EUROPA_CHRISTI + b.EUROPA_CHRISTI, a.WIOSNA + b.WIOSNA,
        a.KONFEDERACJA + b.KONFEDERACJA, a.KUKIZ15 + b.KUKIZ15,
        a.POLSKA_FAIR_PLAY + b.POLSKA_FAIR_PLAY
      ))
    ))
    val suma = woj.map(x=> (x._1,  x._2.KOALICJA_EUROPEJSKA 
      + x._2.LEWICA_RAZEM + x._2.POLEXIT + x._2.JEDNOŚĆ_NARODU 
      + x._2.PIS + x._2.EUROPA_CHRISTI + x._2.WIOSNA 
      + x._2.KONFEDERACJA + x._2.KUKIZ15 + x._2.POLSKA_FAIR_PLAY))

    val procent = woj.zip(suma).map(x => (x._1._1, x._1._2, x._2._2)).map(x => (x._1, x._2 match{
      //case Wynik(a,b,c,d,e,f,g,h,i,j,k) => List(x._1, b.toDouble/x._3, c/x._3, d/x._3, e/x._3, f/x._3, g/x._3, h/x._3, i/x._3, j/x._3, k/x._3)
      case Wynik(a,b,c,d,e,f,g,h,i,j,k) => List(b, c, d, e, f, g, h, i, j, k)
        .map( n => ((n.toDouble/x._3)*100).toInt )
    }, x._3 ))
    val mini = procent.map(x => (x._2(0) - x._2(4)).abs).min
    //wyniki.map(_.toList).map(x=>(x(1)-x(5)).abs).min//.map(x => (x(0),x(1),x(5)))
    val ktore = procent.filter(x => x._2(0)-x._2(4) == mini).map(x => x._2 match{ 
      case List(b,c,d,e,f,g,h,i,j,k) =>  List(x._1, b,c,d,e,f,g,h,i,j,k)
      //s"" + x._1 + s" $b $c $d $e $f $g $h $i $j $k" 
      //Wynik(x._1, b, c, d, e, f, g, h, i, j, k).map(x => "" + województwa.find(n => n(1) = x.ID))
    }).map(
      x => ((województwa.find(n => n.min == x(0)) match{
        case Some(n) => n.nazwa
        case None => ""
      }), x(1), x(2), x(3), x(4), x(5), x(6), x(7), x(8), x(9), x(10)
      )
    ).map(x => " " + x._1 + ": KOALICJA_EUROPEJSKA: " + x._2 + "%, LEWICA_RAZEM: " + x._3 + 
    "%, POLEXIT: " + x._4 + "%, JEDNOŚĆ_NARODU: " + x._5 + "%, PIS: " + x._6 + 
    "%, EUROPA_CHRISTI: " + x._7 + "%, WIOSNA: " + x._8 + "%, KONFEDERACJA: " + x._9 + 
    "%, KUKIZ15: " + x._10 + "%, POLSKA_FAIR_PLAY: " + x._11 + "%"
    )
    
    
    //println(wyniki)
    println(woj)
    println(suma)
    println(procent)
    println(mini)
    println(ktore)  
    //println(wyniki.head)
  }

}
