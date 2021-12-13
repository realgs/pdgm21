object l6 {
  def reverseList[A](list: List[A]): List[A] =
    def reverseListIn[A](innerList: List[A], resultList: List[A]): List[A] =
      if innerList == Nil then resultList else reverseListIn(innerList.tail, innerList.head :: resultList)
    reverseListIn(list, List())

  //  task 1
  def eachNElement[A](list: LazyList[A], n: Int, m: Int): List[A] =
    def eachNElementIn(listIn: LazyList[A], index: Int, result: List[A]): List[A] =
      if index < m then
        if index % n == 0 then eachNElementIn(listIn.tail, index + 1, listIn.head :: result) else eachNElementIn(listIn.tail, index + 1, result)
      else reverseList(result)
    eachNElementIn(list, 0, List())

  // task 2
  def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], operator: String): LazyList[Int] =
    (list1, list2) match
      case (h1 #:: t1, h2#::t2) => operator match
        case "+" => (h1 + h2) #:: lazyExecute(t1, t2, operator)
        case "-" => (h1 - h2) #:: lazyExecute(t1, t2, operator)
        case "*" => (h1 * h2) #:: lazyExecute(t1, t2, operator)
        case "/" => (h1 / h2) #:: lazyExecute(t1, t2, operator)
        case _ => LazyList.empty
      case (_, LazyList()) => list1
      case (LazyList(), _) => list2
      case _ => LazyList.empty

  // task 3
  def duplicate[A](toDuplicate: List[A], repeats: List[Int]): List[A] =
    def duplicateIn(toDuplicateIn: List[A], repeatsIn: List[Int], result: List[A]): List[A] =
      (toDuplicateIn, repeatsIn) match
        case (h1 :: t1, h2 :: t2) => if h2 == 0 then duplicateIn(t1, t2, result) else duplicateIn(toDuplicateIn, (h2 - 1) :: t2, h1 :: result)
        case (_, Nil) => reverseList(result)
        case (Nil, _) => reverseList(result)
    duplicateIn(toDuplicate, repeats, List())

  // task 4 & 5
  trait Debug {
    def debugName(): String =
      this.getClass().getName();

    def displayVar(arg: java.lang.reflect.Field) =
      arg.setAccessible(true)
      (arg.getName(), arg.getType(), arg.get(this))

    def debugVars() =
      this.getClass().getDeclaredFields().toList.map(arg => displayVar(arg))
  }

  def main() = {
    println("eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3): " + eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3))
    println("eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4): " + eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4))

    println("lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), \"+\").take(4).toList: " + lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), "+").take(4).toList)

    println("duplicate(List(1, 2, 3), List(0, 3, 1, 4)): " + duplicate(List(1, 2, 3), List(0, 3, 1, 4)))


    class Point(xv: Int, yv: Int) extends Debug {
      var x: Int = xv
      var y: Int = yv
      var a: String = "test"
    }

    val p = Point(1, 2)

    println("p.debugName(): " + p.debugName())
    println("p.debugVars(): " + p.debugVars())

  }

}
