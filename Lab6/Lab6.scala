
package Lab6

import sun.security.util.Debug

import math.Numeric.Implicits.infixNumericOps
import math.Fractional.Implicits.infixFractionalOps
import math.Integral.Implicits.infixIntegralOps



object Lab6 {

  //1

  def eachNElement[A](list: LazyList[A], n: Int, m: Int): LazyList[A] =
    def eachNElementtail[A](list: LazyList[A], current: Int): LazyList[A] =
      if current == m then LazyList() else (list, current % n) match
        case (h #:: t, 0) => h #:: eachNElementtail(t, current + 1)
        case (h #:: t, _) => eachNElementtail(t, current + 1)
        case (LazyList(), 0) => LazyList()

    if n > 0 & m > 0 then eachNElementtail(list, 0) else LazyList()

  //2
  def lazyExecute(list1: LazyList[Double], list2: LazyList[Double], operator: Char): LazyList[Double] =
    (list1, list2) match {
      case (LazyList(), LazyList()) => LazyList()
      case (list1, LazyList()) => list1
      case (LazyList(), list2) => list2
      case (h1 #:: t1, h2 #:: t2) =>
        operator match {
          case '+' => (h1 + h2) #:: lazyExecute(t1, t2, operator)
          case '-' => (h1 - h2) #:: lazyExecute(t1, t2, operator)
          case '*' => (h1 * h2) #:: lazyExecute(t1, t2, operator)
          case '/' => if h2 != 0 then (h1 / h2) #:: lazyExecute(t1, t2, operator) else h1 #:: lazyExecute(t1, t2, operator)
        }
    }


  //3
  def duplicate[A](list: LazyList[A], rep_list: LazyList[Int]): LazyList[A] =
    (list, rep_list) match
      case (h1 #:: _, h2 #:: t2) if h2 > 0 => h1 #:: duplicate(list, (h2 - 1) #:: t2)
      case (_ #:: t1, _ #:: t2) => duplicate(t1, t2)
      case (_, _) => LazyList()

      //4 + 5


  trait Debug {
      def debugName(): String =
        this.getClass.getName

      def debugVars(): Seq[(String, Any, Any)] =
        val list = this.getClass.getDeclaredFields.toList
        def debugVars_Tail(list: List[java.lang.reflect.Field]): List[(String, Any, Any)] =
          list match
            case Nil => Nil
            case head :: tail =>
              head.setAccessible(true)
              (head.getName, head.getType, head.get(this)) :: debugVars_Tail(tail)


        debugVars_Tail(list)


    }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }


  def main(args: Array[String]): Unit = {

    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3) == LazyList(5, 3))
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4) == LazyList(5, 3))
    println(eachNElement(LazyList.from(-110), 6, 5) == LazyList(-110))
    println(eachNElement(LazyList.from(30), -8, 10) == LazyList())

    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), '+') == LazyList(3, 5, 7, 5))
    println(lazyExecute(LazyList(1, 2, 3, 4, 5), LazyList(1, 2, 3, 4, 5), '-') == LazyList(0, 0, 0, 0, 0))
    println(lazyExecute(LazyList(2.5, 3.5), LazyList(-1), '*') == LazyList(-2.5, 3.5))
    println(lazyExecute(LazyList(2, 1, 2, 1), LazyList(0, 0, 0, 2), '/') == LazyList(2, 1, 2, 0.5))


    println(duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)) == LazyList(2, 2, 2, 3))
    println(duplicate(LazyList(true, false), LazyList(2)) == LazyList(true, true))
    println(duplicate(LazyList("Robert", "Kubica", "driver", "blyskawica"), LazyList(1, 1, 1, 1)) == LazyList("Robert", "Kubica", "driver", "blyskawica"))

    var p : Point = new Point(3, 4)
    println(p.debugName())
    println(p.debugVars())


  }


}
