/* zadanie 1 */

def addListElements(list : List[Int]): Int =
  if list == Nil then 0
  else list.head + addListElements(list.tail)

addListElements(Nil) == 0
addListElements(List(1)) == 1
addListElements(List(-1, 1)) == 0
addListElements(List(1, 2, 3, 4, 5, 6)) == 21

/* zadanie 2 */

val charBetweenWords = " "

def createSentence(list : List[String], lastCharacter: String): String  =
  if list == Nil then lastCharacter
  else if list.tail != Nil then list.head + charBetweenWords + createSentence(list.tail, lastCharacter)
  else  list.head + createSentence(list.tail, lastCharacter)

createSentence(Nil, ".") == "."
createSentence(List("Ala", "ma", "kota"), ".") == "Ala ma kota."
createSentence(List("Czy", "Ala", "ma", "kota"), "?") == "Czy Ala ma kota?"

/* zadanie 3 */

def findIfNumbersGreaterThanZero(list : List[Double]): Boolean =
  if list == Nil then true
  else if list.head > 0 then findIfNumbersGreaterThanZero(list.tail)
  else false

findIfNumbersGreaterThanZero(List()) == true
findIfNumbersGreaterThanZero(List(1, 5, -1)) == false
findIfNumbersGreaterThanZero(List(1)) == true
findIfNumbersGreaterThanZero(List(-2)) == false

/* zadanie 4 */

def factorial(number: Long): Long =
  def count(number: Long, result: Long): Long =
    if number == 0 then result
    else count(number - 1, result * number)
  count(number, 1)

factorial(0) == 1
factorial(1) == 1
factorial(2) == 2
factorial(8) == 40320
factorial(10) == 3628800