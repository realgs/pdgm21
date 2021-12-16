object Lista6 {
  def main(args: Array[String]): Unit =

    println("zadanie 1")
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).take(10).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).take(10).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).take(4).toList)
    println(eachNElement(LazyList(), 2, 10000000).take(10).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1, 7, 8, 10, 12, 14, 11), 4, 20).take(10).toList)
    println(eachNElement(LazyList(-5, 6, 3, -2, 1, 7, 8, 10, -12, 14, -11), 4, 20).take(10).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1, 7, 8, 10, 12, 14, 11), -1, 20).take(10).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1, 7, 8, 10, 12, 14, 11), 4, -1).take(10).toList)

    println("\nzadanie 2")
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').take(10).toList)
    println(lazyExecute(LazyList(2, 2, 2, 1, 2), LazyList(1, 2, 3, 3), '-').take(10).toList)
    println(lazyExecute(LazyList(2, 2, 2), LazyList(0, 0, 3), '*').take(10).toList)
    println(lazyExecute(LazyList(-10, -10, 10, 10), LazyList(10, 10, 10), '/').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3, 4), LazyList(), '/').take(10).toList)
    println(lazyExecute(LazyList(), LazyList(10, 10, 10), '/').take(10).toList)
    println(lazyExecute(LazyList(), LazyList(), '/').take(10).toList)

    println("\nzadanie 3")
    println(repeatElements(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).take(10).toList)
    println(repeatElements(LazyList(), LazyList(0, 3, 1, 4)).take(10).toList)
    println(repeatElements(LazyList(0, 3, 1, 4), LazyList()).take(10).toList)
    println(repeatElements(LazyList(), LazyList()).take(10).toList)


    println("\nzadanie 4")

    class Point(xv: Int, yv: Int) extends Debug {
      var x: Int = xv
      var y: Int = yv
      var a: String = "foo"
    }

    val p: Point = new Point(1, 2)
    println(p.debugName())

    println("\nZadanie 5")
    println(p.debugVars())


  def eachNElement[A](lList: LazyList[A], n: Int, stop: Int): LazyList[A] =
    def helper(lList: LazyList[A], stop: Int, count: Int): LazyList[A] =
      if (stop == 0 || lList.isEmpty) LazyList.empty
      else if (count % n == 0) lList.head #:: helper(lList.tail, stop - 1, n + 1)
      else
        helper(lList.tail, stop - 1, count + 1)

    if (n <= 0 || stop <= 0 || lList.isEmpty) LazyList.empty
    else
      lList.head #:: helper(lList.tail, stop - 1, 1)


  def chooseAction(int1: Int, int2: Int, char: Char) = {
    char match {
      case '*' => int1 * int2
      case '-' => int1 - int2
      case '+' => int1 + int2
      case '/' => int1 / int2
      case _ => 0
    }
  }


  def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], operation: Char): LazyList[Int] = {
    def lazyExecuteInner(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1, list2) match
        case (head1 #:: tail1, head2 #:: tail2) => chooseAction(head1, head2, operation) #:: lazyExecuteInner(tail1, tail2)
        case (head1 #:: _, LazyList()) => list1
        case (LazyList(), _) => list2

    lazyExecuteInner(list1, list2)
  }


  def repeatElements[A](listBeingRepeated: LazyList[A], listOfRepetitions: LazyList[Int]): LazyList[A] = {
    def repeatElementsTail(list1: LazyList[A], list2: LazyList[Int]): LazyList[A] =
      (list1, list2) match
        case (head1 #:: tail1, 0 #:: tail2) => repeatElementsTail(tail1, tail2)
        case (head1 #:: _, head2 #:: tail2) => head1 #:: repeatElementsTail(list1, (head2 - 1) #:: tail2)
        case (_, _) => LazyList()

    repeatElementsTail(listBeingRepeated, listOfRepetitions)
  }

  trait Debug {
    def debugName(): String =
      getClass.getSimpleName
      // without the packages
      // getClass.getName
      // with packages
    def debugVars(): List[Any] =
      getClass.getDeclaredFields.toList.map(field =>
        field.setAccessible(true)
        (field.getName, field.getType, field.get(this)))
  }


}
