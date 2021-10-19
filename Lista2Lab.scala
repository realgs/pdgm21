package Lista2Lab

object Lista2Lab {


  //zadanie 1
  def sumOfIntegers(l: List[Int]): Int = {
    if (l == Nil) {
      0
    }
    else l.head + sumOfIntegers(l.tail)
  }

  // funkcja length określająca długość listy
  def length[T](l: List[T]): Int = {
    if (l == Nil) {
      0
    } else 1 + length(l.tail)

  }

  //zadanie 2
  def createSentence(l: List[String]): String = {
    length(l) match {
      case 0 => ("Wprowadzono niepoprawną liste!")
      case 1 => l.head
      case 2 => l.head + createSentence(l.tail)
      case _ => l.head + " " + createSentence(l.tail)
    }

  }


  //zadanie 3
  def areElementsPositive(l: List[Int]): Boolean = {
    if l == Nil then true
    else if (l.head > 0) {
      areElementsPositive(l.tail)
    } else false

  }


  //zadanie 4
  def factorial(x: Int): Int = {
    if (x > 1) {
      x * factorial(x - 1)
    } else 1
  }


  def main(args: Array[String]): Unit = {

    //test do mojej funkcji length określającej długość listy
    println("\n Funkcja length 1")
    println(length(List(1,23,4))==3)
    println(length(List(1))==1)
    println(length(Nil)==0)

    //test zadanie 1
    println("\n Zadanie 1")
    println(sumOfIntegers(List(5, 4, 3, 2))==14)
    println(sumOfIntegers(Nil)==0)
    println(sumOfIntegers(List(1))==1)

    //test zadanie 2
    println("\n Zadanie 2")
    println(createSentence(List("Zygmunt", "i","Ania","Mają", "dzieci",".")))
    println(createSentence(List("Adam", "żyje", ".")))
    println(createSentence(List()))

    //test zadanie 3
    println("\n Zadanie 3")
    println(areElementsPositive(List(1, 23, 4, 5, 6, 7, 4))==true)
    println(areElementsPositive(List(1, 23, 4, 5, 6, -7, 4))==false)
    println(areElementsPositive(Nil)==true)

    //test zadanie 4
    println("\n Zadanie 4")
    println(factorial(4)==24)
    println(factorial(5)==120)
    println(factorial(0)==1)
  }


}
