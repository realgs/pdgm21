object Main {

  def summ(i_list: List[Int]): Int = {
    if i_list == Nil then 0
    else i_list.head + summ(i_list.tail)
  }

  def concatenationStrings(s_list: List[String], sep: String = " ", end_symbol: String = "."): String = {
    if s_list.tail == Nil then s_list.head + end_symbol
    else s_list.head + sep + concatenationStrings(s_list.tail, end_symbol = end_symbol)
  }

  def isBiggerThanZero(i_list: List[Int]): Boolean = {
    if i_list == Nil then true
    else if i_list.head <= 0 then false
    else isBiggerThanZero(i_list.tail)
  }

  def factorial(number: Int): Int = {
    if number < 0 then -1
    else if number == 1 || number == 0 then 1
    else number*factorial(number-1)
  }

  def main(args: Array[String]): Unit = {

    //Test cases for 1 task
    println(summ(List(1, 3, 6)) == 10)
    println(summ(List(-1, 5, 6, -1000)) == -990)
    println(summ(List(0, 0, 0, 0)) == 0)

    //Test cases for 2 task
    println(concatenationStrings( List("Ola", "Ma", "Kota"), end_symbol = "!") == "Ola Ma Kota!")
    println(concatenationStrings( List("Hello", "World"), end_symbol = "?") == "Hello World?")
    println(concatenationStrings( List("Example", "Phrase", "dodo")) == "Example Phrase dodo.")


    //Test cases for 3 task
    println(isBiggerThanZero(List(0,2,3,-1)) == false)
    println(isBiggerThanZero(List(0,0,0,0)) == false)
    println(isBiggerThanZero(List(5,5,5,6)) == true)

    //Test cases for 4 task
    println(factorial(-20) == -1)
    println(factorial(0) == 1)
    println(factorial(3) == 6)
    println(factorial(1) == 1)
  }
}