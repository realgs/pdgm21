import java.lang.reflect.Field
import Array._

object Lab6 {

  def eachNElement[A](list: LazyList[A], n: Int, m:Int): LazyList[A] =
    def inner[A](list: LazyList[A],nCount: Int, mCount: Int): LazyList[A] =
      (list,nCount,mCount) match
        case (_,_,0) => LazyList()
        case (h#::t,0,_) => h #:: inner(t,n-1,mCount-1)
        case (h#::t,_,_) => inner(t,nCount-1, mCount-1)
    inner(list,0,m)

  def lazyExecute(list1: LazyList[Int], list2: LazyList[Int], operation: String): LazyList[Int] =
    def multiplication(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1,list2) match
        case (h1#::t1,h2#::t2) => (h1*h2) #:: multiplication(t1,t2)
        case (h#::t,LazyList()) => h #:: multiplication(t,LazyList())
        case (LazyList(),h#::t) => h #:: multiplication(t,LazyList())
        case (LazyList(),LazyList()) => LazyList()
    def division(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1,list2) match
        case (h1#::t1,h2#::t2) => (h1/h2) #:: division(t1,t2)
        case (h#::t,LazyList()) => h #:: division(t,LazyList())
        case (LazyList(),h#::t) => h #:: division(t,LazyList())
        case (LazyList(),LazyList()) => LazyList()
    def addition(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1,list2) match
        case (h1#::t1,h2#::t2) => (h1+h2) #:: addition(t1,t2)
        case (h#::t,LazyList()) => h #:: addition(t,LazyList())
        case (LazyList(),h#::t) => h #:: addition(t,LazyList())
        case (LazyList(),LazyList()) => LazyList()
    def subtraction(list1: LazyList[Int], list2: LazyList[Int]): LazyList[Int] =
      (list1,list2) match
        case (h1#::t1,h2#::t2) => (h1-h2) #:: subtraction(t1,t2)
        case (h#::t,LazyList()) => h #:: subtraction(t,LazyList())
        case (LazyList(),h#::t) => h #:: subtraction(t,LazyList())
        case (_,_) => LazyList()
    operation match
      case ("*") => multiplication(list1,list2)
      case ("/") => division(list1,list2)
      case ("+") => addition(list1,list2)
      case ("-") => subtraction(list1,list2)
      case _ => throw new Exception("unknown operation")

  def duplicate[A](list: LazyList[A], counts: LazyList[Int] ): LazyList[A] =
    def inner[A](elem: A, count: Int, t1: LazyList[A], t2: LazyList[Int]): LazyList[A] =
      count match
        case 0 => duplicate(t1,t2)
        case _ => elem #:: inner(elem,count-1,t1,t2)
    (list,counts) match
      case (h1#::t1,h2#::t2) => inner(h1,h2,t1,t2)
      case _ => LazyList()

  trait Debug {
    def debugName(): String = this.getClass.getSimpleName
    def debugVars() =
      def inner(fieldList: List[Field] ): List[List[String]] =
        fieldList match
          case Nil => Nil
          case h::t => {
            h.setAccessible(true)
            List(h.getName, h.getType.toString, h.get(this).toString)::inner(t)
          }
      inner(getClass.getDeclaredFields.toList)
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]) : Unit = {
    //println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).toList)
    //println(lazyExecute(LazyList(1,2,3,4),LazyList(1,2,3,4,5),"/").toList)
    //println(duplicate(LazyList(1,2,3),LazyList(0,3,2)).toList)
    var p : Point = new Point(3, 4)
    println(p.debugVars())

  }

}
