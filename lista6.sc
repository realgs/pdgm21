import java.lang.reflect.Field
import scala.annotation.tailrec

object list6 {

  //Zadanie 1
  def reverseLazyList[A](xs: LazyList[A]): LazyList[A] =
    def reverseLazyListRec[A](xs: LazyList[A], x: LazyList[A]): LazyList[A] =
      if xs == LazyList() then x
      else reverseLazyListRec(xs.tail, xs.head #:: x)

    reverseLazyListRec(xs, LazyList())

  def reverseList[A](xs: List[A]): List[A] =
    def reverseListRec[A](xs: List[A], x: List[A]): List[A] =
      if xs == List() then x
      else reverseListRec(xs.tail, xs.head :: x)

    reverseListRec(xs, List())

  def eachNElement(list: LazyList[Int], n: Int, m: Int): LazyList[Int] =
    @tailrec
    def eachNElementRec(list: LazyList[Int], index: Int, counter: Int, result: LazyList[Int]): LazyList[Int] =
      (list, index % n, counter) match
        case (LazyList(), _, _) => result
        case (_, _, 0) => result
        case (h #:: t, 0, _) => eachNElementRec(t, index + 1, counter - 1, h #:: result)
        case (h #:: t, _, _) => eachNElementRec(t, index + 1, counter - 1, result)

    reverseLazyList(eachNElementRec(list, 0, m, LazyList()))


  //Zadanie 2
  def operation(a: Int, b: Int, op: Char): Int =
    op match
      case '+' => a + b
      case '-' => a - b
      case '*' => a * b
      case '/' => a / b
      case _ => 0

  def lazyExecute(xs1: LazyList[Int], xs2: LazyList[Int], op: Char): LazyList[Int] =
    @tailrec
    def lazyExecuteRec(xs1: LazyList[Int], xs2: LazyList[Int], result: LazyList[Int]): LazyList[Int] =
      (xs1, xs2) match
        case (LazyList(), LazyList()) => result
        case (LazyList(), hd #:: tl) => lazyExecuteRec(LazyList(), tl, hd #:: result)
        case (hd #:: tl, LazyList()) => lazyExecuteRec(tl, LazyList(), hd #:: result)
        case (hd1 #:: tl1, hd2 #:: tl2) => lazyExecuteRec(tl1, tl2, operation(hd1, hd2, op) #:: result)

    reverseLazyList(lazyExecuteRec(xs1, xs2, LazyList()))


  //Zadanie 3
  def repeatElements[A](xs: LazyList[A], repeats: LazyList[Int]): LazyList[A] =
    def repeatElements(xs: LazyList[A], repeats: LazyList[Int]): LazyList[A] =
      (xs, repeats) match
        case (hd1 #:: tl1, 0 #:: tl2) => repeatElements(tl1, tl2)
        case (hd1 #:: tl1, hd2 #:: tl2) => hd1 #:: repeatElements(xs, (hd2 - 1) #:: tl2)
        case (_, _) => LazyList()

    repeatElements(xs, repeats)


  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  trait Debug {
    //Zadanie 4
    def debugName() =
      getClass.getSimpleName


    //Zadanie 5
    def debugVars() =
      @tailrec
      def debugVarsRec(fields: List[Field], result: List[Any]): List[Any] =
        fields match
          case hd :: tl => hd.setAccessible(true); debugVarsRec(tl, (hd.getName, hd.getType, hd.get(this)) :: result)
          case _ => result

      reverseList(debugVarsRec(getClass.getDeclaredFields.toList, List()))
  }


  def main(args: Array[String]): Unit = {
    println("ZADANIE 1")
    println(eachNElement(LazyList(1, 2, 3, 4, 5), 3, 3).take(5).toList)
    println(eachNElement(LazyList(1, 2, 3, 4, 5), 3, 4).take(5).toList)
    println(eachNElement(LazyList(), 2, 10).take(5).toList)
    println(eachNElement(LazyList.from(0), 1, 10).take(20).toList)
    println(eachNElement(LazyList.from(0), 5, 10).take(5).toList)
    println(eachNElement(LazyList.from(0), 10, 10).take(5).toList)
    println("ZADANIE 2")
    println(lazyExecute(LazyList(1, 2, 3, 4, 5), LazyList(1, 2, 3, 4, 5), '+').take(5).toList)
    println(lazyExecute(LazyList(1, 2, 3, 4, 5), LazyList(1, 2, 3, 4, 5), '-').take(5).toList)
    println(lazyExecute(LazyList(1, 2, 3, 4, 5), LazyList(1, 2, 3, 4, 5), '*').take(5).toList)
    println(lazyExecute(LazyList(1, 2, 3, 4, 5), LazyList(1, 2, 3, 4, 5), '/').take(5).toList)
    println(lazyExecute(LazyList(1, 2, 3, 4, 5), LazyList(), '+').take(5).toList)
    println(lazyExecute(LazyList(), LazyList(1, 2, 3, 4, 5), '+').take(5).toList)
    println("ZADANIE 3")
    println(repeatElements(LazyList(1, 2, 3, 4, 5), LazyList(1, 2, 3, 4, 5)).take(10).toList)
    println(repeatElements(LazyList.from(0), LazyList.from(0)).take(10).toList)
    println(repeatElements(LazyList(1, 2, 3), LazyList(1, 2, 3, 4, 5)).take(5).toList)
    println(repeatElements(LazyList(1, 2, 3, 4, 5), LazyList(1, 2, 3)).take(5).toList)
    println(repeatElements(LazyList(), LazyList(1, 2, 3, 4, 5)).take(5).toList)
    println(repeatElements(LazyList(1, 2, 3, 4, 5), LazyList()).take(5).toList)
    println(repeatElements(LazyList(), LazyList()).take(5).toList)
    var p: Point = new Point(3, 4)
    println("ZADANIE 4")
    println(p.debugName())
    println("ZADANIE 5")
    println(p.debugVars())
  }
}