import scala.annotation.tailrec

//zad1
def sumOfElements(list : List[Int]) : Int = {
  if list == Nil then 0
  else list.head + sumOfElements(list.tail)
}
sumOfElements(List(1,2,3,4)) == 10
sumOfElements(Nil) == 0
sumOfElements(List(1,2,-3,4)) == 4

// zad2
val separator = " "
def makeSentence(list : List[String], char :Char) : String = {
  if list == Nil then (""+char)
  else if list.tail == Nil then list.head + makeSentence(list.tail, char)
  else list.head + separator + makeSentence(list.tail, char)
}
makeSentence(List("Ala", "ma", "kota"),'.') == "Ala ma kota."
makeSentence(List("Ola", "ma", "kota"), '?') == "Ola ma kota?"
makeSentence(List("Kot"),'!') == "Kot!"
makeSentence(Nil, ' ') == " "

//zad3
@tailrec
def isPositive(list : List[Double]) : Boolean = {
  list match {
    case Nil => false
    case List(_) => list.head > 0
    case hd :: tl =>  hd > 0 && isPositive(tl)
  }
}

isPositive(List(1,2,3,4,5)) == true
isPositive(List(-0.00000000002)) == false
isPositive(List(-0.00000000002, 2)) == false
isPositive(List(2,3,4,5,-0.00000000002)) == false
isPositive(Nil) == false

//zad4
def factorial(n : Int) : Int = {
  if n < 0 then throw new Exception("invalid argument, negative number")
  if n == 0  || n == 1 then 1
  else n * factorial(n-1)
}

factorial(3) == 6
factorial(10) == 3628800
factorial(-2)
factorial(4.5)



