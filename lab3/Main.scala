import scala.annotation.tailrec

@main
def main(): Unit =

  //exercise 1 - test
  println(splitBySign(List(-1, 1, -2, 2)) == (List(-1, -2), List(1)))
  println(splitBySign(Nil) == (Nil, Nil))
  println(splitBySign(List(2, 4, 6, 8, 0)) == (Nil, Nil))
  println()

  //exersice 2 - test
  println(calculateLength(List(2, 5, 1, -1)) == 4)
  println(calculateLength(Nil) == 0)
  println(calculateLength(List('a', 'b')) == 2)
  println()

  //exersice 3 - test
  println(concatenateLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6))
  println(concatenateLists(Nil, Nil) == Nil)
  println(concatenateLists(List(2, 5, -12), Nil) == List(2, 5, -12))
  println(concatenateLists(List(), List('a', 'b', 'c')) == List('a', 'b', 'c'))
  println()

def reverseList[T](list: List[T]): List[T] =

  @tailrec
  def reverseHelper[T](list: List[T], reversedList: List[T]): List[T] =

    if list == Nil then reversedList
    else reverseHelper(list.tail, list.head :: reversedList)

  reverseHelper(list, Nil)

def splitBySign(list: List[Int]): (List[Int], List[Int]) =

  @tailrec
  def splitHelper(list: List[Int], listNeg: List[Int], listOddPos: List[Int]): (List[Int], List[Int]) =

    list match {

      case (Nil) => (listNeg, listOddPos)
      case (head :: tail) if head < 0 => splitHelper(tail, head :: listNeg, listOddPos)
      case (head :: tail) if head > 0 && head % 2 == 1 => splitHelper(tail, listNeg, head :: listOddPos)
      case (_) => splitHelper(list.tail, listNeg, listOddPos)
    }

  splitHelper(reverseList(list), Nil, Nil)


//time complexity: O(n)
//space complexity: O(n)
//n is length of list
def calculateLength[T](list: List[T]): Int =

  if list == Nil then 0
  else 1 + calculateLength(list.tail)


//time complexity: O(n)
//space complexity: O(n)
//n = list1.length + list2.length
def concatenateLists[T](list1: List[T], list2: List[T]): List[T] =

  (list1, list2) match
    case (Nil, _) => list2
    case (_, Nil) => list1
    case (_) => list1.head :: list2.head :: concatenateLists(list1.tail, list2.tail)
