import scala.::

@main
def main: Unit = {

  //exercise 1 - test
  println(sum(List(2,2,2,5)) == 11)
  println(sum(Nil) == 0)
  println(sum(List(-2, 2, -5, 5)) == 0)
  println()

  //exercise 2 - test
  println(makeSentence(List("Let's", "play", "a game"), '.'))
  println(makeSentence(List("egzample"), '.'))
  println(makeSentence(Nil, '?'))
  println()


  //exercise 3 - test
  println(checkPositive(List(1, 2, 2, 1, 35254)) == true)
  println(checkPositive(Nil) == true)
  println(checkPositive(List(0)) == false)
  println(checkPositive(List(2.5, -2, 0)) == false)
  println()

  //exercise 4 - test
  println(factorial(3) == 6)
  //println(factorial(-2)) //throws exception
  println(factorial(0) == 1)
  println(factorial(1) == 1)
  println()
}

def sum(nrList: List[Int]): Int = {

  if(nrList == Nil) 0
  else nrList.head + sum(nrList.tail)
}

val wordsSeparator = " "
def makeSentence(strList: List[String], sign: Char): String = {

  if(strList == Nil) "" + sign
  else if(makeSentence(strList.tail, sign) == ("" + sign)) strList.head + makeSentence(strList.tail, sign)
  else strList.head + wordsSeparator + makeSentence(strList.tail, sign)
}

def checkPositive(nrList: List[Double]): Boolean = {

  if(nrList == Nil) true
  else if(nrList.head <= 0) false
  else checkPositive(nrList.tail)
}

def factorial(n: Int): Int = {

  if(n < 0) throw Exception("Factorial from negative number undefined")

  if(n == 0) 1
  else n * factorial(n - 1)
}
