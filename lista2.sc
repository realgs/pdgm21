//Magdalena Sołtysiak
//zadanie 1

def sum(xs:List[Int]): Int =
  if xs == Nil then 0
  else xs.head + sum(xs.tail);

sum(List(1,2,3)) == 6
sum(List()) == 0
sum(List(-2,-4,6,-8,-9)) == -17

//zadanie 2
val tmpString = " ";
def stringSum(xs:List[String], x:String): String =
  if xs == Nil then x
  else xs.head + tmpString + stringSum(xs.tail, x);

stringSum(List("Ola", "ma", "kota"), "!")
stringSum(List(), "?")
stringSum(List("Ala", "Ola", "i", "Ania"),"!")


//zadanie 3
def checkNumber(xs:List[Int]): Boolean =
  if xs == Nil then true
  else
    if xs.head > 0 then checkNumber(xs.tail)
    else false;

checkNumber(List(1,2,3,4))
checkNumber(List(-3,6,-4))
checkNumber(List())


//zadanie 4
def factorial(x:Int):Int =
  if x < 0 then throw new Exception("ujemny argument")
  if x == 0 then 1
  else x * factorial(x-1);


factorial(4)
factorial(-5)
factorial(12)



