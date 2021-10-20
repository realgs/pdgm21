//Szymon Sawczuk

//Zadanie 1
val sum = (list:List[Int]) => {
  def sumIter(list:List[Int], result:Int):Int =
    list match
      case Nil => result
      case _ => sumIter(list.tail, result + list.head)

  sumIter(list, 0)

}

sum(List())
sum(List(2, 3, 4))
sum(List(2, -2, -1))
sum(List(3))

//Zadanie 2
val separator = " "

val listToString = (list:List[String]) => {
  def listToStringIter(list:List[String], result:String):String =
    list match
      case Nil => result
      case first::Nil => listToStringIter(Nil, result + first)
      case first::second::Nil => listToStringIter(second::Nil, result + first)
      case _ => listToStringIter(list.tail, result + list.head + separator)

  listToStringIter(list, "")

}

listToString(List("Ala", "ma", "kota", "!"))
listToString(List())
listToString(List("!"))
listToString(List("Hello", "!"))

//Zadanie 3
val isPositive = (list:List[Double]) => {
  def isPositiveIter(list:List[Double], result:Boolean):Boolean =
    list match
      case Nil => result
      case _ =>
        if list.head > 0 then isPositiveIter(list.tail, true)
        else false

  isPositiveIter(list, false)
}

isPositive(List())
isPositive(List(-2, 3, 1.5))
isPositive(List(2.3, 0, 5.6))
isPositive(List(2.3, 9, 8.9))

//Zadanie 4
val factorial = (number:Int) => {
  def factorialInner(number: Int, result:Int):Int =
    number match
      case 0 => result
      case _ => factorialInner(number - 1, result * number)

  if number >= 0 then factorialInner(number, 1)
  else throw new Exception("Not natural number!")
}

factorial(0)
factorial(5)
factorial(8)
