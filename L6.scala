object L6
{
  def eachNElement[A](llistToCheck: LazyList[A], whichElement: Int, lastElement: Int): LazyList[A] =
    def eachNElementHelper(llist: LazyList[A], elementCount: Int, lastElementCount: Int): LazyList[A] =
      (lastElementCount, elementCount == whichElement, llist) match
        case (0, _, _) => LazyList()
        case (_, true, h #:: t) => h #:: eachNElementHelper(t, 1, lastElementCount - 1)
        case (_, false, h #:: t) => eachNElementHelper(t, elementCount + 1, lastElementCount - 1)
        case (_, _, LazyList()) => LazyList()
    if whichElement > 0 && lastElement > 0 then eachNElementHelper(llistToCheck, whichElement, lastElement) else LazyList()

  def performOperation(element1: Int, element2: Int, operator: Char): Int =
    operator match
      case '+' => element1 + element2
      case '-' => element1 - element2
      case '*' => element1 * element2
      case '/' => if element2 != 0 then element1 / element2 else element1
      case _ => element1

  def lazyExecute(llistToExecute1: LazyList[Int], llistToExecute2: LazyList[Int], operation: Char): LazyList[Int] =
    def lazyExecuteHelper(llist1: LazyList[Int], llist2: LazyList[Int]): LazyList[Int] =
      (llist1, llist2) match
        case (h1 #:: t1, h2 #:: t2) => performOperation(h1, h2, operation) #:: lazyExecuteHelper(t1, t2)
        case (h1 #::_, LazyList()) => llist1
        case (LazyList(), _) => llist2
    lazyExecuteHelper(llistToExecute1, llistToExecute2)

  def lrepeat[A](llistToRepeat: LazyList[A], llistNumberOfRepeats: LazyList[Int]): LazyList[A] =
    def lrepeatHelper(llistRepeats: LazyList[Int], llistRepeated: LazyList[A]): LazyList[A] =
      (llistRepeats, llistRepeated) match
        case (0 #:: t1, _ #:: t2) => lrepeatHelper(t1, t2)
        case (h1 #:: t1, h2 #:: _) => h2 #:: lrepeatHelper((h1 - 1) #:: t1, llistRepeated)
        case (_, LazyList()) => LazyList()
        case (LazyList(), _) => LazyList()
    lrepeatHelper(llistNumberOfRepeats, llistToRepeat)

  trait Debug {
    def debugName() =
      this.getClass().getName()

    def getInfoField(field: java.lang.reflect.Field) =
      field.setAccessible(true)
      List(field.getName, field.getType, field.get(this))

    def debugVars=
      this.getClass.getDeclaredFields.toList.map(getInfoField)
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class DefinitelyNotPoint(agev: Int, importantV: Double, namev: String, xv: Int, yv: Int) extends Debug {
    var age: Int = agev
    var importantValue: Double = importantV
    var name: String = namev
    var point: Point = Point(xv, yv)
  }

  def main(args: Array[String]) : Unit =
  {
    println("\neachNElement")
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).take(10).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).take(10).toList)
    println(eachNElement(LazyList(), 2, 10000000).take(10).toList)
    println(eachNElement(LazyList.from(1), 2, 10000000).take(10).toList)
    println(eachNElement(LazyList.from(2), 3, 10000000).take(10).toList)

    println("\nlazyExecute")
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(1, 2), '-').take(10).toList)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(0, 0, 3), '*').take(10).toList)
    println(lazyExecute(LazyList(15, 20, 35, 40), LazyList(5, 20, 10), '/').take(10).toList)
    println(lazyExecute(LazyList(15, 20, 35, 40), LazyList(), '/').take(10).toList)
    println(lazyExecute(LazyList(), LazyList(5, 20, 10), '/').take(10).toList)
    println(lazyExecute(LazyList(), LazyList(), '/').take(10).toList)
    println(lazyExecute(LazyList.from(1), LazyList.from(1), '/').take(10).toList)

    println("\nlrepeat")
    println(lrepeat(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).take(10).toList)
    println(lrepeat(LazyList.from(0), LazyList.from(0)).take(10).toList)
    println(lrepeat(LazyList(), LazyList(0, 3, 1, 4)).take(10).toList)
    println(lrepeat(LazyList(0, 3, 1, 4), LazyList()).take(10).toList)
    println(lrepeat(LazyList(), LazyList()).take(10).toList)

    println("\ndebugName")
    var p : Point = new Point(3, 4)
    var objectToCheck :  DefinitelyNotPoint =  DefinitelyNotPoint(7, 12.5, "GummyBear", 10, 15)
    println(p.debugName())
    println(objectToCheck.debugName())

    println("\ndebugVars")
    println(p.debugVars)
    println(objectToCheck.debugVars)
  }
}
