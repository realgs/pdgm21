object List6 {

  import scala.annotation.tailrec

  def reverse[A](llist: LazyList[A]): LazyList[A] =
    @tailrec
    def reverseTail[A](llist: LazyList[A], result: LazyList[A]): LazyList[A] =
      llist match
        case (head #:: tail) => reverseTail(tail, head #:: result)
        case (LazyList()) => result
    reverseTail(llist, LazyList())


  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] =
    if n <= 0 || m <= 0 then throw new Exception("Wrong input: n and m should be > 0")
    else
      @tailrec
      def eachNElementRec[A](list: LazyList[A], counter: Int, result: LazyList[A]): LazyList[A] =
        list match
          case (head #:: tail) =>
            if counter == m + 1 then reverse(result)
            else if (counter - 1) % n == 0 then eachNElementRec(tail, counter + 1, head #:: result)
            else eachNElementRec(tail, counter + 1, result)
          case (LazyList()) => reverse(result)
      eachNElementRec(list, 1, LazyList())



  def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], operationSign: Char): LazyList[Int] =
    def lazyExecuteRec(list1: LazyList[Int], list2: LazyList[Int], operation: (Int, Int) => Int): LazyList[Int] =
      (list1, list2) match
        case (head1 #:: tail1, head2 #:: tail2) => operation(head1, head2) #:: lazyExecuteRec(tail1, tail2, operation)
        case (LazyList(), _) => list2
        case (_, LazyList()) => list1
    if operationSign == '+' then lazyExecuteRec(list1, list2, (x: Int, y: Int) => x + y)
    else if operationSign == '-' then lazyExecuteRec(list1, list2, (x: Int, y: Int) => x - y)
    else if operationSign == '*' then lazyExecuteRec(list1, list2, (x: Int, y: Int) => x * y)
    else if operationSign == '/' then lazyExecuteRec(list1, list2, (x: Int, y: Int) => x / y)
    else throw new Exception("Wrong input: operation sign should be + or - or * or /")


  def duplicate[A](listToDuplicate: LazyList[A], listWithRepetitionNumbers: LazyList[Int]): LazyList[A] =
    def duplicateRec[A](listToDuplicate: LazyList[A], listWithRepetitionNumbers: LazyList[Int], counter: Int): LazyList[A] =
      (listToDuplicate, listWithRepetitionNumbers) match
        case (head1 #:: tail1, head2 #:: tail2) =>
          if counter < head2 then head1 #:: duplicateRec(listToDuplicate, listWithRepetitionNumbers, counter + 1)
          else duplicateRec(tail1, tail2, 0)
        case _ => LazyList()
    duplicateRec(listToDuplicate, listWithRepetitionNumbers, 0)



  trait Debug {

    def debugName() = getClass.getSimpleName

    def debugVars() =
      def debugVarsRec(listOfFileds: List[java.lang.reflect.Field]): List[Any] =
        listOfFileds match
          case head :: tail =>
            head.setAccessible(true)
            (head.getName, head.getType.getName, head.get(this)) :: debugVarsRec(tail)
          case Nil => Nil
      debugVarsRec(getClass.getDeclaredFields.toList)


  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }


  class Player(namev: String, agev: Int, clubv: String, disciplinev: String ) extends Debug {
    var name: String = namev
    var age: Int = agev
    var club: String = clubv
    var discipline: String = disciplinev
  }


  def main(args: Array[String]): Unit = {

    println("EachNElement tests:")
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList)
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1, 10).toList)
    //println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 0, 10).toList)
    //println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 1, 0).toList)
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 10, 100).toList)
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 3, 1).toList)
    println(eachNElement(LazyList(), 10, 100).toList)
    println(eachNElement(LazyList.from(10), 10, 100).take(100).toList)
    println()
    println()


    println("LazyExecute tests:")
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').toList)
    println(lazyExecute(LazyList(), LazyList(2, 3, 4, 5), '+').toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(), '+').toList)
    println(lazyExecute(LazyList(), LazyList(), '+').toList)
    println(lazyExecute(LazyList.from(10), LazyList.from(1), '+').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '/').toList)
    println(lazyExecute(LazyList(10, 20, 30), LazyList(1, 1, 5, 5), '/').toList)
    //println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '&').toList)
    println()
    println()


    println("Duplicate tests:")
    println(duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).toList)
    println(duplicate(LazyList(), LazyList(0, 3, 1, 4)).toList)
    println(duplicate(LazyList(1, 2, 3), LazyList()).toList)
    println(duplicate(LazyList(1, 2, 3, 4, 5), LazyList(1, 1, 1, 4)).toList)
    println(duplicate(LazyList("aa", "bb"), LazyList(1, 3, 1, 4)).toList)
    println(duplicate(LazyList.from(1), LazyList.from(1)).take(10).toList)
    println()
    println()



    println("Debug tests:")
    var p : Point = new Point(3, 4)
    println(p.debugName())
    println(p.debugVars())
    println()

    var player1: Player = new Player("Robert Lewandowski", 33, "Bayern Monachium", "football")
    println(player1.debugName())
    println(player1.debugVars())
    println()

  }
}
