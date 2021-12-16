import scala.annotation.tailrec

object lab6 {

  //Zadanie 1

  def eachNElement[A](list: LazyList[A], n: Int, m: Int) =
    def eachNElementHelper[A](list: LazyList[A], i: Int, index: Int): LazyList[A] =
      if index == 0 then LazyList()
      else
        (list, i) match
          case (list, 0) => list.head #:: eachNElementHelper(list.tail, n - 1, index - 1)
          case (list, _) => eachNElementHelper(list.tail, i - 1, index - 1)
          case (LazyList(), _) => LazyList()

    if list == LazyList() then LazyList()
    else list.head #:: eachNElementHelper(list, n, m)

  //Zadanie 2

  def lazyExecutue(list1: LazyList[Int], list2: LazyList[Int], operator: Char) =
    @tailrec
    def lazyExecuteHelper(list1: LazyList[Int], list2: LazyList[Int], acc: LazyList[Int]): LazyList[Int] =
      (list1, list2, operator) match
        case (h1 #:: t1, h2 #:: t2, '+') => lazyExecuteHelper(t1, t2, (h1 + h2) #:: acc)
        case (h1 #:: t1, h2 #:: t2, '-') => lazyExecuteHelper(t1, t2, (h1 - h2) #:: acc)
        case (h1 #:: t1, h2 #:: t2, '*') => lazyExecuteHelper(t1, t2, (h1 * h2) #:: acc)
        case (h1 #:: t1, h2 #:: t2, '/') => lazyExecuteHelper(t1, t2, (h1 / h2) #:: acc)
        case (h1 #:: t1, LazyList(), _) => lazyExecuteHelper(t1, list2, h1 #:: acc)
        case (LazyList(), h2 #:: t2, _) => lazyExecuteHelper(list1, t2, h2 #:: acc)
        case (LazyList(), LazyList(), _) => acc.reverse

    lazyExecuteHelper(list1, list2, LazyList())

  //Zadanie 3

  def duplicate[A](list: LazyList[A], list1: LazyList[Int]) =
    @tailrec
    def duplicateHelper[A](list: LazyList[A], list1: LazyList[Int], counter: Int, result: LazyList[A]): LazyList[A] =
      (list, list1) match
        case (head #:: tail, head1 #:: tail1) =>
          if counter < head1 then duplicateHelper(list, list1, counter + 1, head #:: result)
          else duplicateHelper(tail, tail1, 0, result)
        case (LazyList(), _) => result.reverse
        case (_, LazyList()) => result.reverse

    duplicateHelper(list, list1, 0, LazyList())

  trait Debug {

    //Zadanie 4

    def debugName() =
      this.getClass.getSimpleName

    //Zadanie 5

    def debugVars() =
      val list = this.getClass.getDeclaredFields.toList
      def debugVarsHelper(list: List[java.lang.reflect.Field]): List[(String, Any, Any)] =
        list match
          case (head :: tail) =>
            head.setAccessible(true)
            (head.getName(), head.getType(), head.get(this)) :: debugVarsHelper(tail)
          case Nil => Nil

      debugVarsHelper(list)
  }


  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }


  def main(args: Array[String]): Unit = {

    println("Zadanie 1")
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList == List(5, 3))
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList == List(5, 3))
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 5).toList == List(5, 3, 1))

    println("\nZadanie 2")
    println(lazyExecutue(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').toList == List(3, 5, 7, 5))
    println(lazyExecutue(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '-').toList == List(-1, -1, -1, 5))
    println(lazyExecutue(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '*').toList == List(2, 6, 12, 5))
    println(lazyExecutue(LazyList(6, 6, 20), LazyList(2, 3, 4, 5), '/').toList == List(3, 2, 5, 5))

    println("\nZadanie 3")
    println(duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)) == List(2, 2, 2, 3))
    println(duplicate(LazyList(0, 3, 1, 4), LazyList(1, 2, 3)) == List(0, 3, 3, 1, 1, 1))


    println("\nZadanie 4 + 5")

    var p: Point = new Point(3, 4)
    println(p.debugName())
    println(p.debugVars())
  }

}
