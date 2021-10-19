//zadanie 1
def sum(xs: List[Int]): Int =
  if xs == Nil then 0 else
    xs.head + sum(xs.tail)

sum(List(1,2,3,4,5)) == 15
sum(Nil) == 0
sum(List(-2,-1,0,1,2)) == 0


//zadanie 2
def makeSentence(xs: List[String]): String =
  if xs == Nil then "" else
    if xs.tail == Nil then xs.head else
      if xs.tail.tail == Nil then xs.head + makeSentence(xs.tail) else
        xs.head + " " + makeSentence(xs.tail)

makeSentence(List("Ala","ma","kota",".")) == "Ala ma kota."
makeSentence(Nil) == ""
makeSentence(List("Hi","!")) == "Hi!"



//zadanie 3
def isGreaterThan0(xs: List[Int]): Boolean =
  if xs == Nil then true else
    if xs.head < 0 then false else
      isGreaterThan0(xs.tail)

isGreaterThan0(Nil) == true
isGreaterThan0(List(1,2,3,4,5,6,7)) == true
isGreaterThan0(List(1,2,3,-1,4,5)) == false


//zadanie 4
def factorial(n: Int): Int =
  if n == 0 then 1 else
    n * factorial(n - 1)

factorial(0) == 1
factorial(1) == 1
factorial(6) == 720
