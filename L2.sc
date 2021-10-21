def sum(list: List[Int]): Int = {
  if list == Nil then 0
  else list.head + sum(list.tail)
}

sum(List(1, 2, 3, 4)) == 10
sum(List(-15, 4, 3, 8)) == 0

val space = " "
def makeSentence(list: List[String], endOfSentence: String): String = {
  if list == Nil then ""

  else if list.tail == Nil then list.head + endOfSentence
  else list.head + space + makeSentence(list.tail, endOfSentence)
}

makeSentence(List("My", "name", "Yuliia"), "!")
makeSentence(List("I", "have", "a", "dog"), ".")

def isPositive(list: List[Int]): Boolean = {
  if list == Nil then true
  else if list.head > 0 then isPositive(list.tail) else false
}
isPositive(List(1, 2, 3, 4)) == true
isPositive(List(-15, 4, 3, 8)) == false

def factorial(n: Int): Int = {
  if n <= 0 then 0 else if n == 1 then 1
  else n * factorial(n - 1)
}

factorial(5) == 120
factorial(1) == 1
factorial(0) == 0
factorial(-15) == 0
