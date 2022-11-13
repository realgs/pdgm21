import scala.annotation.tailrec

object List6 {

  // task 1

  def reverseList(list: LazyList[Int]): LazyList[Int] = {
    @tailrec
    def reverseListInner(newList: LazyList[Int], oldList: LazyList[Int]): LazyList[Int] =
      oldList match
        case LazyList() => newList
        case head #:: tail => reverseListInner(head #:: newList, tail)
    reverseListInner(LazyList(), list)
  }

  def eachNElement(numberList: LazyList[Int], n: Int, m: Int): LazyList[Int] = {
    @tailrec
    def eachNElementInner(numberListToChoose: LazyList[Int], resultList: LazyList[Int], position: Int): LazyList[Int] =
      if position <  m && !numberListToChoose.isEmpty then
        if (position)%n == 0 then eachNElementInner(numberListToChoose.tail, numberListToChoose.head #:: resultList, position + 1)
        else eachNElementInner(numberListToChoose.tail, resultList, position + 1)
      else resultList
    if (numberList.isEmpty || n <= 0 || m <= 0) then LazyList() else reverseList(eachNElementInner(if !numberList.tail.isEmpty then numberList.tail else LazyList(), numberList.head #:: LazyList(), 1))
  }


  // task 2

  def makeOperation(number1: Int, number2: Int, operator: Char): Int =
    operator match
      case '+' => number1 + number2
      case '-' => number1 - number2
      case '*' => number1 * number2
      case '/' => if number2 == 0 then number1 else number1 / number2
      case _ => number1

  def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], operation: Char): LazyList[Int] = {
    def lazyExecuteInner(remainingList1: LazyList[Int], remainingList2: LazyList[Int]): LazyList[Int] =
      (remainingList1, remainingList2) match
        case (head1 #:: tail1, head2 #:: tail2) => makeOperation(head1, head2, operation) #:: lazyExecuteInner(tail1, tail2)
        case (head1 #:: _, LazyList()) => remainingList1
        case (LazyList(), _) => remainingList2
    lazyExecuteInner(list1, list2)
  }


  // task 3

  def repeatListElements[A](listToRepeat: LazyList[A], listNumberOfRepeats: LazyList[Int]): LazyList[A] = {
    def repeatListElementsInner(remainingListToRepeat: LazyList[A], remainingListOfRepeats: LazyList[Int]): LazyList[A] =
      (remainingListToRepeat, remainingListOfRepeats) match
        case (head1 #:: tail1, 0 #:: tail2) => repeatListElementsInner(tail1, tail2)
        case (head1 #:: _, head2 #:: tail2) => head1 #:: repeatListElementsInner(remainingListToRepeat, (head2 - 1) #:: tail2)
        case (_, _) => LazyList()
    repeatListElementsInner(listToRepeat, listNumberOfRepeats)
  }


  // task 4 + 5

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  trait Debug {
    def debugName(): String =
      getClass.getSimpleName

    def debugVars(): List[Any] =
      getClass.getDeclaredFields.toList.map(field =>
        field.setAccessible(true)
        (field.getName, field.getType, field.get(this)))
  }


  def main(args: Array[String]): Unit = {
    println("test 1\n")
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).take(10).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).take(10).toList)
    println(eachNElement(LazyList(), 2, 10000000).take(10).toList)
    println(eachNElement(LazyList.from(1), 2, 10000000).take(10).toList)
    println(eachNElement(LazyList.from(2), 3, 10000000).take(10).toList)

    println("\ntest 2\n")
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(1, 2), '-').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(0, 0, 3), '*').take(10).toList)
    println(lazyExecute(LazyList(15, 20, 35, 40), LazyList(5, 20, 10), '/').take(10).toList)
    println(lazyExecute(LazyList(15, 20, 35, 40), LazyList(), '/').take(10).toList)
    println(lazyExecute(LazyList(), LazyList(5, 20, 10), '/').take(10).toList)
    println(lazyExecute(LazyList(), LazyList(), '/').take(10).toList)
    println(lazyExecute(LazyList.from(1), LazyList.from(1), '/').take(10).toList)

    println("\ntest 3\n")
    println(repeatListElements(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).take(10).toList)
    println(repeatListElements(LazyList.from(0), LazyList.from(0)).take(10).toList)
    println(repeatListElements(LazyList(), LazyList(0, 3, 1, 4)).take(10).toList)
    println(repeatListElements(LazyList(0, 3, 1, 4), LazyList()).take(10).toList)
    println(repeatListElements(LazyList(), LazyList()).take(10).toList)

    println("\ntest 4 + 5\n")
    var p : Point = new Point(3, 4)
    println(p.debugName())
    println(p.debugVars())
  }
}
