//Szymon Bak

//zadanie 1
def sum(list: List[Int]): Int =
  if list == Nil then 0
  else list.head + sum(list.tail)

sum(List(1, 2, 3, 4, 5)) == 15
sum(List()) == 0
sum(List(-1, 1, -2, 2, -3, 3)) == 0

//zadanie 2
val separator = " "
def createString(list: List[String], endChar: String): String =
  if list == Nil then endChar
  else if list.tail == Nil then list.head + createString(list.tail, endChar)
  else list.head + separator + createString(list.tail, endChar)

createString(List("Hello", "World"), "!") == "Hello World!"
createString(List(), "?") == "?"
createString(List("Ala", "ma", "kota"), ".") == "Ala ma kota."

//zadanie 3
def areAllNumbersPositive(list: List[Int]): Boolean =
  def areAllNumberPositiveHelper(list: List[Int]): Boolean =
    if list == Nil then true
    else if list.head > 0 then areAllNumberPositiveHelper(list.tail)
    else false
  if list == Nil then false //empty list returns false
  else areAllNumberPositiveHelper(list)

areAllNumbersPositive(List(1, 2, 3, 4, 5)) == true
areAllNumbersPositive(List(5, 4, 3, 2, 1, 0, 2)) == false
areAllNumbersPositive(List()) == false

//zadanie 4
def calculateFactorial(number: Long): Long =
  if number < 0 then throw new Exception("Invalid Argument")
  def calculateFactorialAccum(number: Long, accumulator:Long ): Long =
    if number <= 1 then accumulator
    else calculateFactorialAccum(number - 1, accumulator * number)
  calculateFactorialAccum(number, 1)

calculateFactorial(5) == 120
//calculateFactorial(-5) Exception "Invalid Argument"
calculateFactorial(0) == 1
calculateFactorial(4) == 24
