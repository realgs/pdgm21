
import scala.annotation.tailrec
object lab6 {


  def eachNElement[A](list: LazyList[A], n: Int, end_index: Int): LazyList[A] =
    def eachNElementHelper[A](list: LazyList[A], counter: Int, index: Int): LazyList[A] =
      if  index == 0 then LazyList()
      else
        (list, counter) match
          case(LazyList(),_) => LazyList()
          case(list, 0) => list.head #:: eachNElementHelper(list.tail, n-1, index - 1)
          case(list, _) => eachNElementHelper(list.tail, counter-1, index - 1)

    if list == LazyList() then LazyList()
    else list.head #:: eachNElementHelper(list, n, end_index)


  def lazyExecute(firstList: LazyList[Int], secondList: LazyList[Int], operator: (Int, Int) => Int): LazyList[Int] =
    @tailrec
    def lazyListExecuteHelper(firstList: LazyList[Int], secondList: LazyList[Int], result: LazyList[Int]): LazyList[Int] =
      (firstList, secondList) match
        case (_, LazyList()) => result.reverse
        case (LazyList(), _) => result.reverse
        case(first, second) => lazyListExecuteHelper(first.tail, second.tail, operator(first.head, second.head) #:: result)

    lazyListExecuteHelper(firstList, secondList, LazyList())

  val + = (a: Int, b: Int) => a+b
  val - = (a: Int, b: Int) => a-b
  val * = (a: Int, b: Int) => a*b
  val / = (a: Int, b: Int) => a/b

  def duplicate[A](list: List[A], duplicationList: List[Int]): List[A] =
    def duplicateHelper[A](list: List[A], duplicationList: List[Int], counter: Int): List[A] =
      (list, duplicationList, counter) match
        case(Nil, _, _ ) => Nil
        case(_, Nil, _) => Nil
        case (list, duplicationList, 0) => duplicateHelper(list.tail, duplicationList.tail, if duplicationList.tail != Nil then duplicationList.tail.head else 0)
        case (list, duplicationList, _) => list.head :: duplicateHelper(list, duplicationList, counter - 1)
    duplicateHelper(list, duplicationList, duplicationList.head)


  trait Debug{
    def debugName() =
      this.getClass.getName

    def debugVars() =
      val list = this.getClass.getDeclaredFields.toList
      def debugVarsHelper[A](list: List[java.lang.reflect.Field]): List[List[String]] =
        list match
          case Nil => Nil
          case(head :: tail) =>{
            list.head.setAccessible(true)
            List(list.head.getName.toString, list.head.getType.toString, list.head.get(this).toString) :: debugVarsHelper(tail)
          }
      debugVarsHelper(list)
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit = {

    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList)
    println(eachNElement(LazyList(), 2, 4).toList)


    println(lazyExecute(LazyList(1, 2, 3), LazyList(2,3,4,5), +).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2,3,4,5), -).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2,3,4,5), *).toList)
    println(lazyExecute(LazyList(6, 6, 20), LazyList(2,3,4,5), /).toList)

    println(duplicate(List(1, 2, 3), List(0, 3, 1, 4)))
    println(duplicate(List(0, 3, 1, 4), List(1, 2, 3)))

    var p : Point = new Point(3, 4)
    println(p.debugName())
    println(p.debugVars())
  }
}
