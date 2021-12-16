import scala.annotation.tailrec

object lista6 {

  def eachNElement[A](list: LazyList[A], step: Int, end: Int) =
    if step <= 0 then throw new IllegalArgumentException("step must be positive")
    def eachNRec(list: LazyList[A], i: Int, endCounter: Int): LazyList[A] =
      if endCounter <= 0 || list.isEmpty then LazyList()
      else if i == 0 then list.head #:: eachNRec(list.tail, step - 1, endCounter - 1)
      else eachNRec(list.tail, i - 1, endCounter - 1)

    eachNRec(list, 0, end)

  def lazyZipWithCallback[A, B](callback: (A, B) => A|B)(firstList: LazyList[A], secondList: LazyList[B]): LazyList[A|B] =
    (firstList, secondList) match
      case (first, LazyList()) => first
      case (LazyList(), second) => second
      case (hd1 #:: tl1, hd2 #:: tl2) => callback(hd1, hd2) #:: lazyZipWithCallback(callback)(tl1, tl2)

  def arithmeticOperatorCharToFunction(operator: Char): (Double, Double) => Double =
    operator match
      case '+' => (a, b) => a + b
      case '-' => (a, b) => a - b
      case '*' => (a, b) => a * b
      case '/' => (a, b) => a / b

  def lazyExecute(firstList: LazyList[Double], secondList: LazyList[Double], operator: Char): LazyList[Double] =
    lazyZipWithCallback(arithmeticOperatorCharToFunction(operator))(firstList, secondList)

  def duplicate[A](elements: LazyList[A], counts: LazyList[Int]): LazyList[A] =
    (elements, counts) match
      case (elemHd #:: elemTl, countHd #:: countTl) =>
        if countHd <= 0 then duplicate(elemTl, countTl)
        else elemHd #:: duplicate(elements, (countHd - 1) #:: countTl)
      case _ => LazyList()

  trait Debug:
    def debugName(): String =
      getClass().getName()

    def debugVars() =
      for(field <- getClass().getDeclaredFields()) yield {
        field.setAccessible(true)
        Array(field.getName(), field.getGenericType(), field.get(this))
      }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit =
    println(eachNElement(LazyList(5,6,3,2,1), 2, 3).force)
    println(eachNElement(LazyList(5,6,3,2,1), 2, 4).force)
    println(eachNElement(LazyList(5,6,3,2,1), 2, 5).force)
    println(eachNElement(LazyList(5,6,3,2,1), 1, 3).force)
    println(eachNElement(LazyList(5,6,3,2,1,0,-1,-2), 3, 7).force)

    println("-------------")

    println(lazyExecute(LazyList(1,2,3), LazyList(2,3,4,5), '+').force)
    println(lazyExecute(LazyList(1,2,3), LazyList(2,3,4,5), '/').force)
    println(lazyExecute(LazyList(1,2,3,4,5,6,7), LazyList(2,3,4), '-').force)

    println("-------------")

    println(duplicate(LazyList(1,2,3), LazyList(0,3,1,4)).force)
    println(duplicate(LazyList(1,2,3,4,5,6), LazyList(0,3,1,4)).force)

    println("-------------")

    var p : Point = new Point(3, 4)
    println(p.debugName())
    for(devvar <- p.debugVars()) yield println(devvar.mkString(" "))
}
