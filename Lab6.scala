import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer

object Lab6 {
  def eachNElement(list: LazyList[Int], n: Int, m: Int): LazyList[Int] =
    @tailrec
    def eachNElementHelper(elements: LazyList[Int], counter: Int, result: LazyList[Int]): LazyList[Int] =
      if n >= 1 && m >= 1 && m <= list.size then
        if (m == counter) then result
        else if (counter % n == 0) then eachNElementHelper(elements.tail, counter + 1, elements.head #:: result)
        else eachNElementHelper(elements.tail, counter + 1, result)
      else throw new IllegalArgumentException("Niepoprawny argument")

    eachNElementHelper(list, 0, LazyList()).reverse


  val + = (a: Int, b: Int) => a + b
  val - = (a: Int, b: Int) => a - b
  val * = (a: Int, b: Int) => a * b
  val / = (a: Int, b: Int) => a / b

  def lazyExecute(firstList: LazyList[Int], secondList: LazyList[Int], operator: ((Int, Int) => Int)): LazyList[Int] =
    @tailrec
    def lazyExecuteHelper(firstList: LazyList[Int], secondList: LazyList[Int], result: LazyList[Int]): LazyList[Int] =
      (firstList, secondList) match
        case (LazyList(), LazyList()) => result
        case (LazyList(), secondList) => secondList.reverse #::: result
        case (firstList, LazyList()) => firstList.reverse #::: result
        case (h1 #:: t1, h2 #:: t2) => lazyExecuteHelper(t1, t2, operator(h1, h2) #:: result)

    lazyExecuteHelper(firstList, secondList, LazyList()).reverse


  def duplicate(list: List[Int], repeats: List[Int]): List[Int] =
    def duplicateHelper(list: List[Int], repeats: List[Int], result: List[Int]): List[Int] =
      if (list.size > repeats.size) then throw new IllegalArgumentException("Lista z iloscia duplikatow jest zbyt krotka")
      else
        (list, repeats) match
          case (Nil, _) => result
          case (_, Nil) => result
          case (h1 :: t1, h2 :: t2) => if (h2 > 0) then duplicateHelper(list, (h2 - 1) :: t2, h1 :: result)
                                       else duplicateHelper(t1, t2, result)

    duplicateHelper(list, repeats, Nil).reverse

  trait Debug {
    def debugName(): String =
      getClass.getName

    def debugVars(): List[List[Any]] = {
      val listFields = getClass.getDeclaredFields
      val result = ListBuffer[List[Any]]()
      for (field <- listFields) {
        field.setAccessible(true)
        result += List(field.getName, field.getType, field.get(this))
        field.setAccessible(false)
      }

      result.toList
    }

  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit = {

    println(eachNElement(LazyList[Int](5,6,3,2,1),2, 3).toList)
    println(eachNElement(LazyList[Int](5,6,3,2,1),2, 4).toList)
    println(eachNElement(LazyList[Int](5,6,3,2,1),2, 5).toList)

    println(lazyExecute(LazyList[Int](1,2,3), LazyList[Int](2,3,4,5), +).toList)
    println(lazyExecute(LazyList[Int](1,2,3,4), LazyList[Int](3,4,5), -).toList)
    println(lazyExecute(LazyList[Int](1,2,3,4), LazyList[Int](2,3,4,5), *).toList)
    println(lazyExecute(LazyList[Int](1,2,3), LazyList[Int](2,3,4,5), /).toList)

    println(duplicate(List(1,2,3), List(0,3,1,4)))
    //println(duplicate(List(1,2,3), List(0,3)))
    println(duplicate(List(1,2,3,4), List(0,3,1,4)))
    println(duplicate(List(1,2,3,4), List(2,3,1,4)))

    var p : Point = new Point(3, 4);
    println(p.debugName());
    println(p.debugVars())

  }
}
