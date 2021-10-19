object main {
  // Task 1
  // Computational complexity - O(n^2)
  // Memory complexity - O(n)
  def splitBySign(list: List[Int]): (List[Int], List[Int]) = {
    def checkIfListCorrect(list: List[Int]): Boolean = {
      if list == Nil then return true
      else if list.head >= 0 && list.head % 2 == 0 then return false
      else return checkIfListCorrect(list.tail)
    }

    def splitIntoLists(list: List[Int], negList: List[Int], posList: List[Int]): (List[Int], List[Int]) = {
      if list == Nil then return (negList, posList)
      else if list.head < 0 then return splitIntoLists(list.tail, negList ::: List(list.head), posList)
      else return splitIntoLists(list.tail, negList, posList ::: List(list.head))
    }

    if !checkIfListCorrect(list) then throw new Exception("Incorrect sequence")
    else return splitIntoLists(list, List(), List())
  }

  // Task 2
  // Computational complexity - O(n)
  // Memory complexity - O(1)
  def lengthOfList[A](list: List[A]): Int = {
    def calculateLength[A](list: List[A], i: Int): Int = {
      if list == Nil then return i
      else return calculateLength(list.tail, i+1)
    }
    return calculateLength(list, 0)
  }

  // Task 3
  // Computational complexity - O(n^2)
  // Memory complexity - O(n)
  def joinLists[A](listA: List[A], listB: List[A]): List[A] = {
    def concatenateA[A](listA: List[A], listB: List[A], finalList: List[A]): List[A] = {
      if listA == Nil then return finalList ::: listB
      else return concatenateB(listA.tail, listB, finalList ::: List(listA.head))
    }

    def concatenateB[A](listA: List[A], listB: List[A], finalList: List[A]): List[A] = {
      if listB == Nil then return finalList ::: listA
      else return concatenateA(listA, listB.tail, finalList ::: List(listB.head))
    }

    return concatenateA(listA, listB, List())
  }

  def main(args: Array[String]): Unit = {
    println(splitBySign(List(-3, -6, 7, -9, 13)))
    println(splitBySign(List()))
    println(splitBySign(List(3, 5, 7)))
    //println(splitBySign(List(0)))
    println()

    println(lengthOfList(List(5, 4, 3, 2)))
    println(lengthOfList(List()))
    println(lengthOfList(List(List("Ala", "Bartek"), List("Celina"))))
    println()

    println(joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)))
    println(joinLists(List(), List()))
    println(joinLists(List(5, 3, 4, 5, 6), List()))
  }
}
