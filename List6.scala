import scala.annotation.tailrec

object List6 {

  def eachNElement[A](list: LazyList[A], nEach: Int, endIndex: Int): LazyList[A] =
    @tailrec
    def counter[A](tail: LazyList[A], res: LazyList[A], steps: Int, distance: Int): LazyList[A] =
      (distance, tail) match
        case(_, LazyList()) => res.reverse
        case(-1, _) => res.reverse
        case(distance, h#::tail) => if steps == nEach then counter(tail, h#::res, 1, distance-1)
                                    else counter(tail, res, steps+1, distance-1)
    counter(list, LazyList(), nEach, endIndex)

  def lazyExecute(left: LazyList[Int], right: LazyList[Int], operator: Char): LazyList[Int] =
    @tailrec
    def iterator(leftTail: LazyList[Int], rightTail: LazyList[Int], res: LazyList[Int]): LazyList[Int] =
      def operation(left: Int, right: Int): Int =
        operator match
          case '+' => left + right
          case '-' => left - right
          case '*' => left * right
          case '/' => if right == 0 then throw Exception("dividing by zero")
                      else left / right
          case _ => throw Exception("Unsupported operator exception")
      (leftTail, rightTail) match
        case (LazyList(), LazyList()) => res.reverse
        case (LazyList(), h#::tail) => iterator(leftTail, tail, operation(0, h)#::res)
        case (h#::tail, LazyList()) => iterator(tail, rightTail, operation(h, 0)#::res)
        case (lh#::ltail, rh#::rtail) => iterator(ltail, rtail, operation(lh, rh)#::res)
    iterator(left, right, LazyList())
/*W zadaniu 1 i 2 wykorzystuje LazyList zamiast Stream poniewaz Stream jest przestarzaly.
* LazyList zostalo wprowadzone zamiast Stram, poniewaz Stram skladalo sie z leniwej listy instancji klasy Cons,
* ktora jest ewaluowana gorliwie dlatego glowa stream nie byla leniwa, co prowadzilo do bledow przepelniena stosu
* co nie powinno miec miejsca przy strukturze leniwej*/
  def multiplicate[A](elemList: List[A], timesList: List[Int]): List[A] =
    @tailrec
    def tailExtractor(elemTail: List[A], timesTail: List[Int], res:List[A]): List[A] =
      @tailrec
      def duplicator(elem: A, times: Int, res: List[A]): List[A] =
        times match
          case 0 => res
          case _ => duplicator(elem, times-1, elem::res)
      (elemTail, timesTail) match
        case (List(), _) => res.reverse
        case (_, List()) => res.reverse
        case (eh::etail, tt::ttail) => tailExtractor(etail, ttail, duplicator(eh, tt, res))
    tailExtractor(elemList, timesList, List())
/*W zadaniu 3 korzustam z Listy poniewaz wykorzystuje wszystkie elementy po kolei wiec wystarczy mi dostep do glowy
* ktory zapewnie lista, zamiast tworzyc tablice z dostepem do wszystkich komorek kosztem zajmowania pojedynczego duzego bloku pamieci*/

  def main(args: Array[String]): Unit = {

    val lazylist = LazyList(5,6,3,2,1)
    println(eachNElement(lazylist,2,5).force)
    val lazy1 = LazyList(1,2,3)
    val lazy2 = LazyList(2,3,4,5)
    println(lazyExecute(lazy1,lazy2,'+').force)
    println(lazyExecute(lazy1,lazy2,'*').force)
    val list1 = List(1,2,3)
    val list2 = List(0,3,1,4)
    println(multiplicate(list1,list2))

    val p : Point = new Point(3, 4)
    p.debugName()
    p.debugVars()

  }
}
trait Debug{

  def debugName(): Unit =
    println(getClass)

  def debugVars(): Unit =
    var res = List[Any]()
    getClass().getDeclaredFields().toList.foreach { f =>
      f.setAccessible(true)
      res = List((f.getName(), f.getType(), f.get(this)))::res
    }
    println(res.reverse)

}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"

}
