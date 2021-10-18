object Lista2 {

  //Zadanie 1

  val sumList: List[Int] => Int = xs =>
    if xs == Nil then throw new Exception("Given list is empty!")
    else if xs.tail == Nil then xs.head
    else xs.head + sumList(xs.tail)

  //Zadanie 2

  val stringSeparator = " "

  val appendListOfStrings: (List[String], Char) => String = (xs, x) =>
    if x == 0 then throw new Exception("Missing sentence ending symbol!")
    else if xs == Nil then "" + x
    else if xs.tail == Nil then xs.head + x
    else xs.head + stringSeparator + appendListOfStrings(xs.tail, x)

  //Zadanie 3

  val positivityOfList: List[Double] => Boolean = xs =>
    if xs == Nil then throw new Exception("Given list is empty!")
    else if xs.tail == Nil then xs.head > 0
    else xs.head > 0 && positivityOfList(xs.tail)

  //Zadanie 4

  /*val numFactorial: Int => Int = x =>
    if x < 0 then throw new Exception("Podano ujemna liczbe!")
    else if x == 0 then 1
    else x * numFactorial(x-1)*/

  //Rekursja ogonowa
  val numFactorial: Long => Long = x =>
    if x < 0 then throw new Exception("Podano ujemna liczbe!")
    else
      def numFactorialIter(x: Long, ac: Long): Long =
        if x == 0 then ac
        else numFactorialIter(x - 1, x * ac)
      numFactorialIter(x,1)

  def main(args: Array[String]): Unit = {

    //Testy do zadania 1
    println(sumList(List(2,5,6)) == 13)
    println(sumList(List(0,-5,6,-8)) == -7)
    println(sumList(List(2)) == 2)
    //println(sumList(List()))

    //Testy do zadania 2
    println(appendListOfStrings(List("Ala", "ma", "kota"), '.') == "Ala ma kota.")
    println(appendListOfStrings(List("Ala"), '!') == "Ala!")
    println(appendListOfStrings(List(), '.') == ".")
    //println(appendListOfStrings(List("Ala", "ma", "kota"), '0'))

    //Testy do zadania 3
    println(positivityOfList(List(2,5,6)) == true)
    println(positivityOfList(List(2,-5,6)) == false)
    println(positivityOfList(List(2.3,5.6,6.9)) == true)
    println(positivityOfList(List(-2.7,-5.7,6.1)) == false)
    println(positivityOfList(List(-2)) == false)
    println(positivityOfList(List(2)) == true)
    //println(positivityOfList(List()))

    //Testy do zadania 4
    println(numFactorial(4) == 24)
    println(numFactorial(1) == 1)
    println(numFactorial(0) == 1)
    //println(Factorial(-6))
  }
}


