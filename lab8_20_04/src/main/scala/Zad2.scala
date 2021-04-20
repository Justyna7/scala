object Zad2 {

  def main(args: Array[String]): Unit = {
    val linie = io.Source
      .fromResource("nazwiska.txt")
      .getLines.toList
  }

}
