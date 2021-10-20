//1
def sumList(list: List[Int]): Int = {
  if list == Nil then 0
  else list.head + sumList(list.tail)
}

sumList(List(1, 2, 3, 4, 5))
sumList(List(3, 4, -3))
sumList(List())


//2
var space = ' '
def mergeStrings(list: List[String], char: Char): String = {
  if list == Nil then "" + char
  else if list.tail == Nil then list.head + mergeStrings(list.tail, char)
  else list.head + space + mergeStrings(list.tail, char)
}

mergeStrings(List("Ala", "ma", "kota"), '.')
mergeStrings(List("Hello"), '!')
mergeStrings(List(), '?')


//3
def positiveNumList(list: List[Int]): Boolean = {
  if list == Nil then false
  else if list.head <= 0 then false
  else if list.tail == Nil then true
  else positiveNumList(list.tail)
}

positiveNumList(List(1, 2, 3, 4, 5))
positiveNumList(List(1, -2, -3, 4, 5))
positiveNumList(List())


//4
def factorial(n: Int): Int = {
  if n == 0 then 1
  else if n>0 then n * factorial(n-1)
  else throw new Exception("Ujemny argument.")
}

factorial(5)
factorial(2)
factorial(1)
factorial(0)
factorial(-5)
