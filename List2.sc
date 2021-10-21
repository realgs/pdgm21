//Zadanie 1

def listSum(xs: List[Int]): Int =
  if xs == Nil then 0
  else xs.head + listSum(xs.tail)

listSum(List(1,2,3,4,5,6,7,8,9)) == 45
listSum(List(1,-2,3,-4,5,-6,7,-8,9)) == 5
listSum(List())==0

//Zadanie 2

def hardcodedSymbol = " ";;

def joinStrings (xs: List[String], sSymbol: String): String =
  if xs == Nil then sSymbol
  else if xs.tail == Nil then xs.head + sSymbol
  else xs.head + hardcodedSymbol + joinStrings(xs.tail, sSymbol)

joinStrings (List("Jakieś", "krótkie", "zdanie"), ".") == "Jakieś krótkie zdanie."
joinStrings (List("Test"), "!") == "Test!"
joinStrings (List(), ".") == "."
joinStrings (List(), "") == ""

//Zadanie 3

def greaterThanZero(xs: List[Int]): Boolean =
  if xs == Nil then false
  else if xs.head <= 0 then false
  else if xs.tail == Nil then true
  else greaterThanZero (xs.tail)

greaterThanZero(List(1,2,3)) == true
greaterThanZero(List(1,-2,3)) == false
greaterThanZero(List(-2)) == false
greaterThanZero(List()) == false

//Zadanie 4
def fact(x: Int): Int =
  if x < 0 then throw new Exception("A negative argument was given!")
  else if x <= 1 then 1
  else x * fact (x - 1)

fact(0)==1
fact(1)==1
fact(2)==2
fact(10)==3628800
//fact(-12)
