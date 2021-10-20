//1

def sumOfList(xs : List[Int]) : Int =
  if xs == Nil then 0
  else xs.head + sumOfList(xs.tail)

sumOfList(Nil) == 0
sumOfList(List(1, 2, 3, 4)) == 10
sumOfList(List(0, -1)) == -1

//2

def sentence(xs : List[String], x : String) : String =
  if xs == Nil then x
  else if xs.tail == Nil then xs.head + x
  else xs.head + " " + sentence(xs.tail, x)

sentence(List("How", "are", "you"), "?") == "How are you?"
sentence(List(), ".") == "."
sentence(List("Dog"), ";") == "Dog;"

//3

def checkList(xs : List[Int]) : Boolean =
  if xs == Nil then true
  else if xs.head > 0 then checkList(xs.tail)
  else false

checkList(List(1, 0, -2, 4)) == false
checkList(List(1, 2, 3, 4)) == true
checkList(Nil) == true

//4

def power(x : Int) : Int =
  if x == 0 then 1
  else if x > 0 then x * power(x - 1)
  else throw new Exception ("invalid number")

power(0) == 1
power(3) == 6
//power(-2)

