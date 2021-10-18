//zadanie1
def sumList (xs: List[Int]): Int =
  if xs== Nil then 0
  else xs.head + sumList(xs.tail)

sumList(List(1, 2, 3))
sumList(List())
sumList(List(1))

//zadanie2
def sentenceList (xs: List[String]): String =
  if xs == Nil then ""
  else xs.head + " " + sentenceList(xs.tail)

sentenceList(List("ala", "ma", "kota", "."))
sentenceList(List())
sentenceList(List("."))

//zadanie3
def biggerThanZero (xs: List[Int]): Boolean =
  if xs == Nil then true
  else if xs.head <= 0 then false
  else biggerThanZero(xs.tail)

biggerThanZero(List(2,3,0,5))
biggerThanZero(List())
biggerThanZero(List(1,2,3,4))

//zadanie4
def factorial (x: Int): Int =
  if x == 0 then 1
  else x * factorial(x-1)

factorial(0)
factorial(3)
factorial(10)
