//Jakub Kowalczyk

import scala.annotation.tailrec

//Zadanie 1

@tailrec
def go1(list: List[Int], a: Int): Int =
  if list == Nil then a
  else go1(list.tail, a + list.head)

def sum(list: List[Int]): Int =
  go1(list, 0)

sum(List(1,2,3,4,5)) == 15
sum(Nil) == 0
sum(List(-1,-2,-3,-4,-5)) == -15
sum(List(1)) == 1


//Zadanie 2

val spacja = " "
val kropka = "."

@tailrec
def go2(list: List[String], a: String): String =
  if list == Nil then a + kropka
  else go2(list.tail, a + spacja + list.head)

def sentence(list: List[String]): String =
  if list == Nil then ""
  else go2(list.tail, list.head)

sentence(List("Ala", "ma", "kota")) == "Ala ma kota."
sentence(List("Ala")) == "Ala."
sentence(List("a", "b", "c", "d", "e", "f")) == "a b c d e f."
sentence(Nil) == ""

//Zadanie 3

@tailrec
def areGreaterThanZero(list: List[Integer]): Boolean =
  if list == Nil then true
  else (if (list.head > 0) then areGreaterThanZero(list.tail) else false)

areGreaterThanZero(Nil) == true
areGreaterThanZero(List(1,2,3,4,5)) == true
areGreaterThanZero(List(0,0,0,0)) == false
areGreaterThanZero(List(-1,-2,-3,-4,-5)) == false

//Zadanie 4

@tailrec
def go4(n: Int, a: Int): Int =
  if n == 0 then a
  else go4(n-1, a*n)

def factorial(n: Int): Int =
  if n < 0 then throw new Exception("negative number")
  else go4(n, 1)

factorial(1) == 1
factorial(5) == 1*2*3*4*5
factorial(0) == 1
factorial(10) == 1*2*3*4*5*6*7*8*9*10
//factorial(-5)
