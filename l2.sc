import scala.annotation.tailrec

//zadanie 1

def sumInts(xs: List[Int]): Int =
  @tailrec
  def sumIntsRec(xs: List[Int], acc: Int): Int =
    if xs == Nil then acc else sumIntsRec(xs.tail, acc + xs.head)

  sumIntsRec(xs, 0)

sumInts(List(0,1,2,3,4,5,6)) == 21
sumInts(Nil) == 0
sumInts(List(-1,1,0,0,5,5)) == 10

//zadanie 2

def concatStrings(xs: List[String], endChar : Char): String =
  @tailrec
  def concatStringsRec(xs: List[String], acc: String):String =
    if xs == Nil then acc + endChar
    else if xs.tail == Nil then acc + xs.head + endChar
    else concatStringsRec(xs.tail, acc + xs.head + " ")

  concatStringsRec(xs, "")

concatStrings(List("Ala", "ma", "kota"), '!') == "Ala ma kota!"
concatStrings(Nil, '.') == "."
concatStrings(List("kot"), 'y') == "koty"

//zadanie 3
@tailrec
def isAllPositive(xs: List[Double]) : Boolean =
  if xs == Nil then true
  else if xs.head > 0 then isAllPositive(xs.tail)
  else false

isAllPositive(List(1,2,3,4,5)) == true
isAllPositive(List(1,2,3,0,4,5)) == false
isAllPositive(List(-1)) == false
isAllPositive(List(1)) == true
isAllPositive(Nil) == true

//zadanie 4

def factorial(n: Int): Int =
  @tailrec
  def factorialRec(n: Int, acc: Int): Int =
    if n == 0 then acc
    else if n > 0 then factorialRec(n - 1, acc * n)
    else throw new Exception(s"ujemny argument $n")

  factorialRec(n, 1)

factorial(0) == 1
factorial(1) == 1
factorial(2) == 2
factorial(3) == 6
factorial(10) == 3628800
