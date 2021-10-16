object Main {
  // Task 1
  def sum(list: List[Int]): Int = {
    if list == Nil then return 0
    else return list.head + sum(list.tail)
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
