/**
 * @author Jakub Szwedowicz
 * @version 1.0
 *          date: 12.12.2021
 *          email: kuba.szwedowicz@gmail.com
 */

import math.Numeric.Implicits.infixNumericOps

object List6 {
  def main(args: Array[String]): Unit =
    testTask1
    testTask2
    testTask3
    testTask4
    testTask5

  def testTask1: Unit =
    println("=============== Task 1 ===============")
    println("Printing default 7 elements of list")
    lList.take(7).foreach(println)
    println("Printing elements of eachNElement(llist, 3, 7)")
    lazy val targetLlist = eachNElement(lList, 3, 7)
    targetLlist.take(10).foreach(println)

  // Task 1
  def eachNElement[A](lList: LazyList[A], n: Int, end: Int): LazyList[A] =
    def helper(lList: LazyList[A], end: Int, toSkip: Int): LazyList[A] =
      if (end == 0 || lList.isEmpty) LazyList.empty
      else if (toSkip != 0) helper(lList.tail, end - 1, toSkip - 1)
      else
        lList.head #:: helper(lList, end - 1, n)

    if (n <= 0 || end <= 0 || lList.isEmpty) LazyList.empty
    else
      lList.head #:: helper(lList.tail, end - 1, n - 1)

  // Task 2
  def testTask2: Unit =
    println("=============== Task 2 ================")
    println("Printing 4 elements of default lList")
    lList.take(4).foreach(println)
    println("Printing 4 elements of default slList")
    slList.take(4).foreach(println)

    println("Printinf 5 elements of lazyExecute(lList, slList, 'add')")
    lazy val targetlList = lazyExecute(lList, slList, "add")
    targetlList.take(5).foreach(println)

  def lazyExecute[A: Numeric](flList: LazyList[A], slList: LazyList[A], operation: String): LazyList[A] =
    (flList, slList) match {
      case (h1 #:: t1, h2 #:: t2) =>
        operation match {
          case "add" => (h1 + h2) #:: lazyExecute(t1, t2, operation)
          case "sub" => (h1 - h2) #:: lazyExecute(t1, t2, operation)
          case "mul" => (h1 * h2) #:: lazyExecute(t1, t2, operation)
          case "div" => (h1.toDouble / h2.toDouble).asInstanceOf[A] #:: lazyExecute(t1, t2, operation)
          case _ => LazyList.empty
        }
      case _ => LazyList.empty
    }


  // Task 3
  def testTask3: Unit =
    println("=============== Task 3 ================")
    println("Printing first 4 elements of lList")
    lList.take(4).foreach(println)

    println("Printing duplicate(lList.LazyLit(0, 3, 1).take(5).foreach(println)")
    duplicate(lList, LazyList(0, 3, 1)).take(5).foreach(println)

  def duplicate[A](toDuplicate: LazyList[A], duplicateScalars: LazyList[Int]): LazyList[A] =
    def helper(toDuplicate: LazyList[A], duplicateScalars: LazyList[Int], counter: Int): LazyList[A] =
      if (counter == 0)
        (toDuplicate, duplicateScalars) match
          case (h1 #:: t1, h2 #:: t2) => helper(t1, t2, h2)
          case (_) => LazyList.empty
      else
        toDuplicate.head #:: helper(toDuplicate, duplicateScalars, counter - 1)

    helper(toDuplicate, duplicateScalars.tail, duplicateScalars.head)


  // Task 4
  def testTask4: Unit =
    println("=============== Task 4 ================")

    var f: Foo = new Foo()
    println("This is class: " + f.debugName())

  trait Debug:
    def debugName(): String =
      getClass.getSimpleName

    def debugVars() =
      getClass.getDeclaredFields.toList.map(field =>
        field.setAccessible(true)
        (field.getName, field.getType, field.get(this)))

  case class Foo() extends Debug {
    var x: Int = 1
    var y: Int = 0
    var a: String = "test"
  }


  // Task 5
  def testTask5: Unit =
    println("=============== Task 5 ================")
    var f: Foo = new Foo()
    println("Fields of class Foo: " + f.debugVars())


  private final lazy val lList: LazyList[Int] = 1 #:: 2 #:: lList.tail.map(n => n + 1)
  private final lazy val slList: LazyList[Int] = 2 #:: 4 #:: slList.tail.map(n => n + (n >> 1))
}
