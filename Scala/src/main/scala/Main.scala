object Main {
  // Task 1
  def sum(list: List[Int]): Int = {
    if list == Nil then return 0
    else return list.head + sum(list.tail)
  }

  // Helper for Task 2
  def isSequenceCorrect(list: List[String]): Boolean = {
    if list == Nil then return false
    else if list.tail == List(".") || list.tail == List("?") || list.tail == List("!") then return true
    else return isSequenceCorrect(list.tail)
  }

  // Task 2
  def concatenateStrings(list: List[String]): String = {
    def appendWords(list: List[String]): String = {
      if list.head == "." || list.head == "?" || list.head == "!" then return list.head
      return " " + list.head + appendWords(list.tail)
    }

    if !isSequenceCorrect(list) then throw new Exception("Incorrect sequence")
    else return list.head + appendWords(list.tail)
  }

  // Task 3
  def isPositive(list: List[Int]): Boolean = {
    if list == Nil then return true
    else if list.head <= 0 then return false
    else return isPositive(list.tail)
  }

  // Task 4
  def calculateFactorial(n: Int): Int = {
    if n < 0 then throw new Exception("Cannot calculate negative factorial")
    else if n == 0 then return 1
    else return n * calculateFactorial(n-1)
  }

  def main(args: Array[String]): Unit = {
    println(sum(List(1, 3, 6)))
    println(sum(List(-4, 89, 1, -56)))
    println(sum(List()))
    println()

    println(concatenateStrings(List("Hello", "World", "!")))
    println(concatenateStrings(List("Did", "I", "code", "this", "task", "properly", "?")))
    println(concatenateStrings(List("You", "are", "wonderful", ".")))
    // The calls below will throw an exception
    //println(concatenateStrings(List()))
    //println(concatenateStrings(List("HELLO")))
    //println(concatenateStrings(List(".")))
    //println(concatenateStrings(List("HELLO", ".", "WORLD")))
    println()

    println(isPositive(List(1, 5, 7)))
    println(isPositive(List(5, -2, 3)))
    println(isPositive(List()))
    println()

    // Exception will be thrown here
    //println(calculateFactorial(-1))
    println(calculateFactorial(0))
    println(calculateFactorial(1))
    println(calculateFactorial(5))
    println()
  }
}
