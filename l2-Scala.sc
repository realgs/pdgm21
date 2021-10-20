//Zadanie 1
def sum(xs: List[Int]): Int =
  if(xs == Nil) then 0
  else xs.head + sum(xs.tail)

sum(List(1,2,3,11,25)) == 42
sum(List(-5,7,8,9)) == 19
sum(List()) == 0
sum(Nil) == 0

//Zadanie 2
val space = " "

def sentence(xs: List[String], x: String): String =
  if(xs == Nil) then x
  else space + xs.head + sentence(xs.tail, x)

sentence(List("Polacz", "te", "slowa"), ".")
sentence(List("Jestem", "zdaniem", "wykrzyknikowym"), "!")
sentence(List(), "?")

//Zadanie 3
def isPositive(xs: List[Int]): Boolean =
  if xs == Nil then true
  else if xs.head > 0 then isPositive(xs.tail)
  else false

isPositive(List(2,4,5,6))
isPositive(List(0,5,8,6,5,8))
isPositive(List(-1,5,9,11,55))

//Zadanie 4
def factorial(x: Int): Int =
  if x < 0 then throw new Exception("Ujemny argument!")
  else if x == 0 then 1
  else x*factorial(x-1)

factorial(6)
factorial(12)
factorial(0)
//factorial(-5)
