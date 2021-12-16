package Test

object L6 {
  def eachNElement[A](list: LazyList[A], n: Int, m: Int) = {
    def eachNElementHelper[A](list: LazyList[A], concurrentPosition: Int, m: Int,
                              finalList: LazyList[A]
                             ): LazyList[A] = {
      (list, m) match
        case (_, 0) => finalList
        case (LazyList(), _) => finalList
        case (head #:: tail, m) => if concurrentPosition == n then eachNElementHelper(tail, 1, m - 1, head #:: finalList) else eachNElementHelper(tail, concurrentPosition + 1, m - 1, finalList)
    }

    eachNElementHelper(list, n, m, LazyList()).reverse
  }

  def operation(e1: Int, e2: Int, operator: Char): Int = {
    operator match {
      case '-' => e1 - e2
      case '+' => e1 + e2
      case '*' => e1 * e2
      case '/' => e1 / e2
    }
  }

  def lazyExecute(listA: LazyList[Int], listB: LazyList[Int], operator: Char): LazyList[Int] = {
    def lazyExecuteHelper(listA: LazyList[Int], listB: LazyList[Int]): LazyList[Int] =
      (listA, listB) match {
        case (h1 #:: t1, h2 #:: t2) => operation(h1, h2, operator) #:: lazyExecuteHelper(t1, t2)
        case (_, LazyList()) => listA
        case (LazyList(), _) => listB
      }

    lazyExecuteHelper(listA, listB)
  }


  def repeat[A](list: LazyList[A], repeatList: LazyList[Int]): List[A] = {
    def repeatHelper[A](list:LazyList[A], repeatList: LazyList[Int],concurrentPosition: Int,finalList: List[A]): List[A] = {
      (list, repeatList) match {
        case (LazyList(), _) => finalList
        case (_, LazyList()) => finalList
        case (h1#::t1, h2#::t2) => if concurrentPosition >= h2 then repeatHelper(t1, t2, 0, finalList) else repeatHelper(list, repeatList, concurrentPosition+1, h1::finalList)
      }
    }
    repeatHelper(list, repeatList,0, List()).reverse
  }

  trait Debug {
    def debugName() = this.getClass().getSimpleName()

    def debugVars() = {
      var bugs = List[List[String]]()

      for (bug <- this.getClass().getDeclaredFields()){
        bug.setAccessible(true)
        val faieldList = List(bug.getName().toString(), bug.getAnnotatedType().toString(), bug.get(this).toString())
        bugs = faieldList :: bugs
      }
      bugs.reverse
    }


  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }


  def main(args: Array[String]): Unit = {

    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList)

    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').toList)
    println(lazyExecute(LazyList(10, 20, 30), LazyList(2, 3, 4, 5), '/').toList)
    println(lazyExecute(LazyList(2, 3, 4, 5), LazyList(1, 2, 3), '+').toList)

    println(repeat(LazyList(1,2,3), LazyList(0,3,1,4,5,6)))
    println(repeat(LazyList(1,2,3,4,5,6,7), LazyList(2,3)))

    var p : Point = new Point(3, 4)
    println(p.debugName())
    println(p.debugVars())

  }

}
