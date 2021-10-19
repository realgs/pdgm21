def sum(list :List[Double]):Double =
  if list == Nil then 0
  else list.head + sum(list.tail)

sum(List(1,2,3,4)) == 10
sum(List(1.0,2.0,1.0,2.0)) == 6.0
sum(List()) == 0

def makeSentence(list :List[String], end :String):String =
  if list == Nil then throw new Exception("List can't be empty empty")
  if list.tail == Nil then list.head + end
  else list.head + " " + makeSentence(list.tail, end)

makeSentence(List("Ala","ma","kota"),".") == "Ala ma kota."
makeSentence(List("Ala"),"!") == "Ala!"
makeSentence(List("Ala","ma",".","kota"),".") == "Ala ma . kota."


def checkIfPositive(list :List[Double]):Boolean =
  if list == Nil then true
  else if list.head > 0.0 then checkIfPositive(list.tail)
  else false

checkIfPositive(List(-1,2,3,4)) == false
checkIfPositive(List(1,2,3,4)) == true
checkIfPositive(List(1.0,2.0)) == true

def factorial(number :Int) = {
  if number < 0 then throw new Exception("Factorial can be use only for positive numbers")
  def factorialInternal(value :Int, sum :Int):Int =
    if value == 0 then sum
    else factorialInternal(value - 1, sum * value)

  factorialInternal(number, 1)
}
factorial(-1)
factorial(0) == 1
factorial(1) == 1
factorial(2) == 2
factorial(4) == 24
factorial(5) == 120

