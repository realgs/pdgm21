
//Zadanie 1

def listSum (list: List[Int]): Int =
  def listSumHelper (list: List[Int], sum: Int): Int =
    if list == Nil then sum
    else listSumHelper(list.tail, sum + list.head )
  listSumHelper (list, 0)

listSum(List(1, 2, 3, 4, 5)) == 15
listSum(List()) == 0
listSum(List(1, 123, 1, 2, 5)) == 132

//Zadanie 2

object Constants{
  val separator: Char = ' ';
}

def stringFromList(list: List[String], endChar: Char) : String =
  def stringFromListHelper (list: List[String], endChar: Char, result: String): String =
    if list == Nil then result
    else if list.head == endChar.toString then result + endChar
    else if result == "" then  stringFromListHelper(list.tail, endChar, (result + list.head))
    else stringFromListHelper(list.tail, endChar, (result + Constants.separator + list.head))
  stringFromListHelper(list, endChar, "")


stringFromList(List("Litwo,", "Ojczyzno", "moja!", "Ty", "jestes", "jak", "zdrowie", "!"), '!')
  == "Litwo, Ojczyzno moja! Ty jestes jak zdrowie!"
stringFromList(List("test", "dzialania", "zadania", "."), '.') == "test dzialania zadania."
stringFromList(List(), ' ') == ""
stringFromList(List("a", "b", "c"), 'a') == "a"
stringFromList(List("W", "B", "?" ,"c"), '?') == "W B?"

//Zadanie 3

def greaterThanZeroList(list: List[Int]): Boolean =
  def greaterThanHelper(list: List[Int]): List[Int] =
    if list == Nil then Nil
    else if list.head > 0 then list.head :: greaterThanHelper(list.tail)
    else Nil
  if list == Nil then false
  else greaterThanHelper(list) == list

greaterThanZeroList(List(1, 2, 3)) == true
greaterThanZeroList(List()) == false
greaterThanZeroList(List(1, 4, 0, 5)) == false
greaterThanZeroList(List(5, -1, 2, 9)) == false

//Zadanie 4

def factorial(n: Int): Int =
  def factorialHelper(n: Int): Int =
    if n == 0 then 1
    else n * factorialHelper(n-1)
  if n < 0 then throw new Exception(s"ujemny argument")
  else factorialHelper(n)


factorial(5) == 120
factorial(0) == 1
factorial(10) == 3628800
// test zwracający wyjątek ujemy argument
//factorial(-1)

