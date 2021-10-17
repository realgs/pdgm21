object lista2 {

  def sumaElementow(xs: List[Int]): Int =
    if (xs == Nil) then 0
    else xs.head + sumaElementow(xs.tail)

  def utworzZdanie(xs: List[String], znak: Char): String =
    if (xs == Nil) then znak.toString
    else if (xs.tail == Nil) xs.head + znak
    else xs.head + ' ' + utworzZdanie(xs.tail, znak)

  def czyDodatnia(xs: List[Int]): Boolean =
    if (xs == Nil) then true
    else if xs.head < 0 then false
    else czyDodatnia(xs.tail)

  def silnia(x:Int) : Int =
    if x < 0 then throw new Exception("ujemny argument")
    else if x == 0 then 1
    else x * silnia(x-1)

  def main(args: Array[String]) = {
    println("testy");

    //zadanie 1
    println("zadanie 1");
    println(sumaElementow(List(-1, 2, 3, 5)) == 9);
    println(sumaElementow(List()) == 0);
    println(sumaElementow(List(1)) == 1);

    //zadanie 2
    println("zadanie 2");
    println(utworzZdanie(List("ala", "ma", "kota"), '.') == "ala ma kota.");
    println(utworzZdanie(List("ok"), '!') == "ok!");
    println(utworzZdanie(List(), '?') == "?");

    //zadanie 3
    println("zadanie 3");
    println(czyDodatnia(List(1,2,3,-4)) == false);
    println(czyDodatnia(List(1)) == true);
    println(czyDodatnia(List(-1)) == false);
    println(czyDodatnia(List()) == true);

    println("zadanie 4");
    println(silnia(0)==1)
    println(silnia(1)==1)
    println(silnia(5)==120)
    //println(silnia(-1)==0) // wyrzuca wyjatek

  }
}