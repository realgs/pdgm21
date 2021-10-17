//Zadanie 1
def totalSum(xs: List[Int]): Int =
  if xs != Nil
  then xs.head + totalSum(xs.tail)
  else 0

totalSum(List())
totalSum(List(1, 2, 3, 4, 5, 6))
totalSum(List(-3, -5))


//Zadanie 2 - program wypisuje string do pojawienia sie kropki. Jesli kropka się nie pojawi to wstawia ją / informuje o niej

def stringifyAddDot (xs: List[String]) : String =
  if xs != Nil && xs.head != "."
  then xs.head + " " + stringifyAddDot(xs.tail)
  else "."

stringifyAddDot(List())
stringifyAddDot(List("This", "is", "a", "correct", "sentence", "."))
stringifyAddDot(List("This", "is", "a", "sentence", ".", "after", "the", "dot"))
stringifyAddDot(List("Here", "program", "inserts", "a", "missing", "dot"))


def stringifyInformDotIsMissing (xs: List[String]) : String =
  if xs != Nil
    then if xs.head != "."
      then xs.head + " " + stringifyInformDotIsMissing(xs.tail)
    else "."
  else "[the dot is missing]"

stringifyInformDotIsMissing(List())
stringifyInformDotIsMissing(List("This", "is", "a", "correct", "sentence", "."))
stringifyInformDotIsMissing(List("This", "is", "a", "sentence", ".", "after", "the", "dot"))
stringifyInformDotIsMissing(List("Here", "program", "inserts", "a", "missing", "dot"))


//Zadanie 3

def areBiggerThanZero(xs: List[Int]): Boolean =
  if xs != Nil
    then if xs.head > 0
      then areBiggerThanZero(xs.tail)
    else false
  else true

areBiggerThanZero(List())
areBiggerThanZero(List(1, 2, 3, 4, 5, 6))
areBiggerThanZero(List(1, 2, 3, 4, -5, 6))


//Zadanie 4

def factorial(n:Int): Int =
  if  n == 0
    then 1
  else n * factorial(n-1)

factorial(4)
factorial(-4)
factorial(0)