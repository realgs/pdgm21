import java.lang.reflect.Type
import scala.collection.mutable.ListBuffer

object Lista6 {

  //zadanie 1
  def eachNElement[A](list: LazyList[A], eachStep: Int, lastElement: Int ) =
    def helper(list: LazyList[A], toNextElement: Int, elementsLeft: Int): LazyList[A] =
      (list,toNextElement,elementsLeft) match
        case (_, _, 0) => LazyList.empty
        case (h#::t, 1, m) => h#::helper(t, eachStep, m - 1)
        case (_#::t, n, m) => helper(t, n - 1, m - 1)
    helper(list,1,lastElement)

  //zadanie 2
  def lazyExecute(firstList: LazyList[Double], secondList: LazyList[Double], operationSymbol: Char) =
    def operation(firstValue: Double, secondValue: Double, operation: Char) =
      operation match
        case '+' => firstValue + secondValue
        case '-' => firstValue - secondValue
        case '*' => firstValue * secondValue
        case '/' => firstValue / secondValue
        case _ => throw new Exception("unsupported math operation !")

    def helper(firstList: LazyList[Double], secondList: LazyList[Double]): LazyList[Double] =
      (firstList, secondList) match
        case (LazyList(), l2) => l2
        case (l1, LazyList()) => l1
        case (h1#::t1, h2#::t2) => operation(h1, h2, operationSymbol)#::helper(t1, t2)
    helper(firstList, secondList)

  //zadanie 3
  def duplicate[A](elementsList: LazyList[A], countsList: LazyList[Int]) =
    def duplicateElement(element: A, count: Int, result: LazyList[A]): LazyList[A] =
      count match
        case 0 => result
        case c => duplicateElement(element, c - 1, element#::result)

    def helper(eList: LazyList[A], cList: LazyList[Int], result: LazyList[A]): LazyList[A] =
      (eList, cList) match
        case (LazyList(), _) => result
        case (_, LazyList()) => result
        case (h1#::t1, h2#::t2) => helper(t1, t2, duplicateElement(h1, h2, result))
    helper(elementsList, countsList, LazyList.empty).reverse

  //zadanie 4 i 5
  trait Debug {
    def debugName() = getClass().getSimpleName()

    def debugVars() =
      val fields = new ListBuffer[(String, Type, Any)]()
      for (field <- getClass().getDeclaredFields())
        field.setAccessible(true)
        fields += ((field.getName(), field.getGenericType(), field.get(this)))
        field.setAccessible(false)
      fields.toList
  }

  def main(args: Array[String]): Unit = {

    //zadanie 1
    val testList1 = LazyList(5, 6, 3, 2, 1)
    println(eachNElement(testList1, 2, 3).force)
    println(eachNElement(testList1, 2, 4).force)
    println(eachNElement(testList1, 2, 5).force)

    //zadanie 2
    val testList2 = LazyList(1.0, 2.0, 3.0)
    val testList3 = LazyList(2.0, 3.0, 4.0, 5.0)
    println(lazyExecute(testList2, testList3, '+').force)
    println(lazyExecute(testList2, testList3, '-').force)

    //zadanie 3
    val testList4 = LazyList(1, 2, 3)
    val testList5 = LazyList(0, 3, 1, 4)
    println(duplicate(testList4, testList5).force)

    //zadanie 4
    class Point(xv: Int, yv: Int) extends Debug {
      var x: Int = xv
      var y: Int = yv
      var a: String = "test"
    }

    var p : Point = new Point(3, 4);
    println(p.debugName())

    //zadanie 5
    println(p.debugVars())

  }
}
