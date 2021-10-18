//task 1

val addElements = (xs: List[Int]) =>
  if xs == Nil then throw new Exception("empty list")
  else
    def addElementsRec(xs: List[Int], result: Int): Int =
      if xs == Nil then result
      else addElementsRec(xs.tail, result + xs.head)
    addElementsRec(xs, 0)


addElements(List(1,1,1)) == 3
addElements(List(1,-2,-1,-5)) == -7
addElements(List(1,-1,-1,1)) == 0
//addElements(List()) == 0


//task 2

val makeSentence = (xs: List[String],x: String) =>
  if xs == Nil then throw new Exception("empty list")
  else if !(x == "!"|| x=="?" || x==".") then throw new Exception("bad ending sign")
  else
    def makeSentenceRec(xs: List[String],x: String):  String =
      if xs.tail == Nil then xs.head + x
      else xs.head + " " + makeSentenceRec(xs.tail, x)
    makeSentenceRec(xs,x)


makeSentence(List("Ala","ma","kota"),".") == "Ala ma kota."
makeSentence(List("Ala","ma","kota"),"!") == "Ala ma kota!"
makeSentence(List("Jaka","jest","pogoda","we","Wroclawiu"),"?") == "Jaka jest pogoda we Wroclawiu?"
//makeSentence(List(),".")
//makeSentence(List("Ala","ma","kota")," ")
//makeSentence(List("Ala","ma","kota"),"..")


//task 3

val greaterThanZero = (xs: List[Double]) =>
  if xs == Nil then throw new Exception("empty list")
  else
    def greaterThanZeroRec(xs: List[Double]): Boolean =
      if xs.head <= 0 then false
      else if xs.tail == Nil then true
      else greaterThanZeroRec(xs.tail)
    greaterThanZeroRec(xs)

greaterThanZero(List(4,3,2.0)) == true
greaterThanZero(List(-4,3,2.0)) == false
greaterThanZero(List(3,3,-6)) == false
//greaterThanZero(List())


//task 4

val factorial = (n: Int) =>
  if n<0 then throw new Exception("wrong input: negative number")
  else if n>12 then throw new Exception("wrong input: number > 12")
  else
    def factorialRec(n: Int, result: Int): Int =
      if n == 0 then result
      else factorialRec(n-1, n*result)
    factorialRec(n, 1)


factorial(0) == 1
factorial(1) == 1
factorial(10) == 3628800
factorial(12) == 479001600
//factorial(-5)
//factorial(13)
