import scala.collection.mutable.HashMap
import scala.collection.mutable.Queue

object Main {
  // Zadanie 1
  def eachNElement[A](elements: LazyList[A], step: Int, endIndex: Int): LazyList[A] = {
    def eachNElementInner(elements: LazyList[A], indexElement: Int): LazyList[A] = {
      if indexElement == endIndex then LazyList()
      else elements match {
        case LazyList() => LazyList()
        case head #:: tail =>
          if indexElement % step == 0 then head #:: eachNElementInner(tail, indexElement+1)
          else eachNElementInner(tail, indexElement+1)
      }
    }

    eachNElementInner(elements, 0)
  }

  // Zadanie 2
  def compute(number1: Float, number2: Float, operator: String): Float = {
    operator match {
      case "+" => number1 + number2
      case "-" => number1 - number2
      case "*" => number1 * number2
      case "/" => number1 / number2
      case _ => throw new Exception("Illegal operator")
    }
  }

  def lazyExecute(list1: LazyList[Float], list2: LazyList[Float], operator: String): LazyList[Float] = {
    (list1, list2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (number #:: tail, LazyList()) => number #:: lazyExecute(tail, LazyList(), operator)
      case (LazyList(), number #:: tail) => number #:: lazyExecute(LazyList(), tail, operator)
      case (number1 #:: tail1, number2 #:: tail2) =>
        compute(number1, number2, operator) #:: lazyExecute(tail1, tail2, operator)
    }
  }

  // Zadanie 3
  def duplicate[A](elements: LazyList[A], howMuchToDuplicate: LazyList[Int]): LazyList[A] = {
    def duplicateInner(copyElement: A, noDuplicates: Int, restElemenets: LazyList[A], restNoDuplicates: LazyList[Int]): LazyList[A] = {
      if noDuplicates == 0 then duplicate(restElemenets, restNoDuplicates)
      else copyElement #:: duplicateInner(copyElement, noDuplicates-1, restElemenets, restNoDuplicates)
    }

    (elements, howMuchToDuplicate) match {
      case (LazyList(), _) => LazyList()
      case (_, LazyList()) => LazyList()
      case (element #:: restElements, noDuplicates #:: restNoDuplicates) =>
        duplicateInner(element, noDuplicates, restElements, restNoDuplicates)
    }
  }

  // Zadanie 4 i 5
  trait Debug {
    def debugName(): String = this.getClass.getSimpleName
    def debugVars(): List[List[String]] = {
      var listFields: List[List[String]] = List()
      for (v <- this.getClass.getDeclaredFields) {
        v.setAccessible(true)
        listFields = listFields ::: List(List(v.getName.toString, v.getType.toString, v.get(this).toString))
      }
      listFields
    }
  }

  // Helper classes
  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class Circle(radiusv: Float) extends Debug {
    var radius: Float = radiusv
    var name: String = "Circle"
  }

  def main(args: Array[String]): Unit = {
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).toList == List(5, 3))
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList == List(5, 3))
    println(eachNElement(LazyList(), 2, 3).toList == List())
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11), 3, 8).toList == List(1, 4, 7))
    println()
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), "+").toList == List(3, 5, 7, 5))
    println(lazyExecute(LazyList(1, 2, 3), LazyList(1, 2), "-").toList == List(0, 0, 3))
    println(lazyExecute(LazyList(3, 3, 3), LazyList(4, 0, 2), "*").toList == List(12, 0, 6))
    println(lazyExecute(LazyList(4, 2, 1), LazyList(1, 2, 4), "/").toList == List(4, 1, 0.25))
    println(lazyExecute(LazyList(), LazyList(), "+").toList == List())
    //println(lazyExecute(LazyList(1), LazyList(1), operator = "//").toList)
    println()
    println(duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)).toList == List(2, 2, 2, 3))
    println(duplicate(LazyList(1, 2, 3, 4), LazyList(3, 2, 0)).toList == List(1, 1, 1, 2, 2))
    println(duplicate(LazyList(), LazyList()) == List())
    println()
    var p: Point = new Point(3, 4)
    println(p.debugName())
    println(p.debugVars())
    var c: Circle = new Circle(5)
    println(c.debugName())
    println(c.debugVars())
  }
}
