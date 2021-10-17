//1
def listSum(list: List[Int]): Int =
  if list==Nil then 0
  else list.head + listSum(list.tail)

listSum(List(1, 2, 3)) == 6
listSum(List(1)) == 1
listSum(List()) == 0

//2
def listLength[A](list: List[A]): Int =
  if list==Nil then 0
  else 1 + listLength(list.tail)

def createSentence(list: List[String]): String =
  if listLength(list)==0 then ""
  else if listLength(list)==1 then list.head
  else if list.tail==List(".") || list.tail==List(".") || list.tail==List("?") then list.head + (list.tail).head
  else list.head + " " + createSentence(list.tail)

createSentence(List("Ala", "ma", "kota", ".")) == "Ala ma kota."
createSentence(List("Ala")) == "Ala"
createSentence(List("Ala", "ma")) == "Ala ma"
createSentence(List(".")) == "."
createSentence(List()) == ""

//3
def positiveCheck(list: List[Int]): Boolean =
  if list==Nil then true
  else if list.head<=0 then false
  else positiveCheck(list.tail)

positiveCheck(List(1, 2, 3)) == true
positiveCheck(List(1, 2, -3)) == false
positiveCheck(List()) == true

def strictPositiveCheck(list: List[Int]): Boolean =
  listLength(list) != 0 && positiveCheck(list)

strictPositiveCheck(List(1, 2, 3)) == true
strictPositiveCheck(List(1, 2, -3)) == false
strictPositiveCheck(List()) == false

//4
def factorial(n: Int): Int =
  if n==0 then 1
  else n*factorial(n-1)

factorial(0) == 1
factorial(1) == 1
factorial(3) == 6
