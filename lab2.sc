import scala.annotation.tailrec

//task 1
def addListElements (xs: List[Int]): Int =
  if xs == Nil then 0 else addListElements(xs.tail) + xs.head

addListElements(Nil) == 0
addListElements(List(0)) == 0
addListElements(List(1, -2, -10, 5)) == -6
addListElements(List(10, 2, 5, 3)) == 20

//task 1 tail recursion
def addListElementsTail (xs: List[Int]): Int = {
  @tailrec
  def addListElementsIter(xs: List[Int], accum: Int): Int =
    if xs == Nil then accum else addListElementsIter(xs.tail, accum + xs.head)

  addListElementsIter(xs, 0)
}

addListElementsTail(Nil) == 0
addListElementsTail(List(0)) == 0
addListElementsTail(List(1, -2, -10, 5)) == -6
addListElementsTail(List(10, 2, 5, 3)) == 20

//task 2

val charInSentence = " "

def createSentence (xs: List[String], x: Char): String =
  if xs == Nil then s"$x"
  else if createSentence(xs.tail, x) != s"$x" then xs.head + charInSentence + createSentence(xs.tail, x)
  else xs.head + createSentence(xs.tail, x)

createSentence(Nil, '.') == "."
createSentence(List("Aliens"), '!') == "Aliens!"
createSentence(List("Does", "free", "will", "exist"), '?') == "Does free will exist?"
createSentence(List("Vanitas", "vanitatum", "et", "omnia", "vanitas"), '.') == "Vanitas vanitatum et omnia vanitas."

//task 2 tail recursion
def createSentenceTail (xs: List[String], x: Char): String = {
  @tailrec
  def createSentenceIter(xs: List[String], x: Char, accum: String): String =
    if xs == Nil then accum + s"$x"
    else if xs.tail == Nil then createSentenceIter(xs.tail, x, accum + xs.head)
    else createSentenceIter(xs.tail, x, accum + xs.head + charInSentence)

  createSentenceIter(xs, x, "")
}

createSentenceTail(Nil, '.') == "."
createSentenceTail(List("Aliens"), '!') == "Aliens!"
createSentenceTail(List("Does", "free", "will", "exist"), '?') == "Does free will exist?"
createSentenceTail(List("Vanitas", "vanitatum", "et", "omnia", "vanitas"), '.') == "Vanitas vanitatum et omnia vanitas."


//task 3
@tailrec
def isListPositive (xs: List[Double]): Boolean =
  if xs == Nil then true
  else if xs.head <= 0.0 then false
  else isListPositive(xs.tail)

isListPositive(Nil) == true
isListPositive(List(0.0)) == false
isListPositive(List(10.0, 2.5, -1.2)) == false
isListPositive(List(110.0, 22.1, 11.2)) == true

//task 4
def factorial (n: Long): Long =
  if n == 0 then 1
  else n * factorial(n - 1)

factorial(0) == 1
factorial(1) == 1
factorial(5) == 120
factorial(12) == 479001600

//task 4 tail recursion
def factorialTail (n: Long): Long = {
  @tailrec
  def factorialIter(n: Long, accum: Long): Long =
    if n == 0 then accum
    else factorialIter(n - 1, n * accum)

  factorialIter(n, 1)
}

factorialTail(0) == 1
factorialTail(1) == 1
factorialTail(5) == 120
factorialTail(12) == 479001600
