//Lobocka Paulina

//1
def sum (list : List[Int]) : Int =
  if list == Nil then 0
  else list.head + sum(list.tail)

sum(List(1, 2, 5)) == 8
sum(Nil) == 0
sum(List(-2, 13, 0)) == 11

//2
def append (list : List[String], char : String) : String =
  if list == Nil  then char
  else if list.tail == Nil then list.head + char
  else list.head + " " + append(list.tail, char)


append(List("testing", "my", "code"), "!") == "testing my code!"
append(List(""), ".") == "."
append(Nil, ".") == "."

//3
def arePositive(list : List[Int]) : Boolean =
  if list == Nil then true
  else if list.head > 0 then arePositive(list.tail)
  else false

arePositive(List(1, 12, 1654, 7)) == true
arePositive(List(-2, -3, 0)) == false
arePositive(Nil) == true

//4
def factorial(nr : Int) : Long =
  if nr < 0 then throw new Exception("negative argument")
  if nr < 2 then 1
  else nr * factorial(nr-1)

factorial(4) == 24
factorial(0) == 1
factorial(10) == 3628800
//factorial(-2)
