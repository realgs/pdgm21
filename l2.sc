//zadanie 1
def add (xs: List[Int]):Int =
  if xs == Nil then 0
  else xs.head + add(xs.tail)

add(List(1,2,3,10))
add(List())
add(List(0))
add(List(-6,5))

//zadanie 2
def sentence (xs: List[String], x: Char):String =
  if xs != Nil then xs.head + " " + sentence(xs.tail,x)
  else x.toString

sentence(List("Ala","ma","kota"),'.')
sentence(List(),'.')
sentence(List("Hello","World"),'!')

//zadanie 3
def moreThanZero (xs:List[Int]):Boolean =
  if xs == Nil then throw new Exception("The list is empty")
  else if xs.head <= 0 then false
  else if xs.tail != Nil then moreThanZero(xs.tail)
  else true

moreThanZero(List(-1,2))
moreThanZero(List(1,2,3,0))
moreThanZero(List())
moreThanZero(List(1,2,3))

//zadanie 4
def factorial (x:Int):Int =
  if x == 0 then 1
  else if x > 0 then x * factorial(x-1)
  else throw new Exception ("Negative argument")

factorial(0)
factorial(-2)
factorial(5)
