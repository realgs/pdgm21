import scala.annotation.tailrec

object l6 {

  // Task 1

  def eachNElement[A](llist: LazyList[A])(n: Int)(m: Int): LazyList[A] =
    @tailrec
    def inner[A](llist: LazyList[A])(iter: Int)(result: LazyList[A]): LazyList[A] =
      (llist, iter) match
        case (h #:: t, k) => if k == m then result.reverse
                             else inner(t)(k + 1)(if k % n == 0 then h #:: result else result)
        case (LazyList(), _) => result.reverse

    inner(llist.tail)(1)(LazyList[A](llist.head))

  // Task 2

  def lazyExecute(list1: LazyList[Int])(list2: LazyList[Int])(operator: (Int, Int) => Int): LazyList[Int] =
    @tailrec
    def inner(list1: LazyList[Int])(list2: LazyList[Int])(result: LazyList[Int]): LazyList[Int] =
      (list1, list2) match
        case (h1 #:: t1, h2 #:: t2) => inner(t1)(t2)(operator(h1, h2) #:: result)
        case (list1, LazyList()) => list1.reverse #::: result
        case (LazyList(), list2) => list2.reverse #::: result

    inner(list1)(list2)(LazyList()).reverse

  // Task 3

  def duplicate[A](list1: LazyList[A])(list2: LazyList[Int]): LazyList[A] =
    @tailrec
    def inner(list1: LazyList[A])(list2: LazyList[Int])(i: Int)(result: LazyList[A]): LazyList[A] =
      (list1, list2, i) match
        case (h1 #:: t1, h2 #:: t2, i) => if i == 0 then inner(t1)(t2)(h2)(result)
                                          else inner(list1)(list2)(i - 1)(h1 #:: result)
        case(_, _, _) => result //if one of lazy lists is empty

    inner(list1)(list2.tail)(list2.head)(LazyList()).reverse

  //Task 4 Task 5

  trait Debug {
    def debugName: String = this.getClass.getName

    def debugVars: List[List[Any]] =
      @tailrec
      def inner(list: List[java.lang.reflect.Field])(result: List[List[Any]]): List[List[Any]] =
        list match
          case h :: t => h.setAccessible(true) // otherwise IllegalAccessException thrown
                         inner(t)(List(h.getName, h.getType, h.get(this)) :: result)
          case Nil => result

      inner(getClass.getDeclaredFields.toList)(Nil).reverse
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  class Person(vornameP: String, nameP: String, idP: Long) extends Debug {
    var vorname: String = vornameP
    var name: String = nameP
    var id: Long = idP
  }

  def main(args: Array[String]): Unit = {
    val llist1 = LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    val llist2 = LazyList(1, 0, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16)
    val llist3 = LazyList(1, 0, 0, 0, 2, 2, 0, 0, 2)
    println(llist1)
    println(llist1.head)
    println(llist1)

    // Task 1
    println("\neachNElement: -----------------------------------------------------------")
    println(eachNElement(LazyList(5, 6, 3, 2, 1))(2)(3).take(10).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1))(2)(4).take(10).toList)
    println(eachNElement(LazyList(5, 6, 3, 2, 1))(7)(4).take(10).toList)
    println(eachNElement(llist1)(2)(16).take(10).toList)
    println(eachNElement(llist1)(2)(15).take(10).toList)

    // Task 2
    println("\nlazyExecute: ------------------------------------------------------------")
    val + = (a: Int, b: Int) => a + b
    val - = (a: Int, b: Int) => a - b
    val * = (a: Int, b: Int) => a * b
    val / = (a: Int, b: Int) => if b != 0 then a / b else 0 //throw new Exception("lazyExecute: division by 0!")
    println(lazyExecute(llist1)(llist3)((a: Int, b: Int) => a + b).take(12).toList)
    println(lazyExecute(llist1)(llist2)(+).take(10).toList)
    println(lazyExecute(llist1)(llist2)(-).take(10).toList)
    println(lazyExecute(llist1)(llist2)(*).take(10).toList)
    println(lazyExecute(llist1)(llist2)(/).take(10).toList)

    // Task 3
    println("\nduplicate: --------------------------------------------------------------")
    println(duplicate(LazyList(1, 2, 3))(LazyList(0, 3, 1, 4)).take(10).toList)
    println(duplicate(llist1)(llist3).take(10).toList)

    // Task 4 Task 5
    println("\ndebug: ------------------------------------------------------------------")
    val point: Point = new Point(1, 2)
    println(point.debugName)
    println(point.debugVars)
    val person1: Person = new Person("Jan", "Kowalski", 1234)
    val person2: Person = new Person("Anna", "Nowak", 1235)
    println(person1.debugName)
    println(person1.debugVars)
    println(person2.debugVars)
  }
}
