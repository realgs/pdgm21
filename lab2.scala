//Piotr Zięba

//Zadanie 1
def sumOfList(xs: List[Int]): Int =
  if xs == Nil then 0
  else xs.head + sumOfList(xs.tail)

sumOfList(Nil) == 0
sumOfList(List(1,2,3,4,5)) == 15
sumOfList(List(0,0,1)) == 1

//Zadanie 2
def wordsToSentence(xs: List[String]): String =
  def wordsToSentenceI(xs: List[String], sSentence: String): String =
    if xs == Nil then sSentence
    else if xs.head == "." then sSentence + "."
    else if sSentence != "" then wordsToSentenceI(xs.tail, sSentence + " " + xs.head)
    else wordsToSentenceI(xs.tail, xs.head)
  wordsToSentenceI(xs, "")

wordsToSentence(List("Ala", "ma", "kota", ".")) == "Ala ma kota."
wordsToSentence(Nil) == ""
wordsToSentence(List(".")) == "."

//Zadanie 3
def greaterThanZero(xs: List[Int]): Boolean =
  if xs == Nil then true
  else if xs.head <= 0 then false
  else greaterThanZero(xs.tail)

greaterThanZero(List(1, 2, 3)) == true
greaterThanZero(List(1, 3, -7)) == false
greaterThanZero(List(0, 42, 84)) == false
greaterThanZero(Nil) == true


//Zadanie 4
def factorial(n: Int): Int =
  if n < 0 then throw new Exception(s"Wrong argument: $n")
  else
    def factorialI(n: Int, a: Int): Int =
      if n == 0 then a
      else factorialI(n-1, n * a)
    factorialI(n, 1)

//factorial(-1)
factorial(0) == 1
factorial(5) == 120
