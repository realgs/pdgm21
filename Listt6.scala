import scala.annotation.tailrec
import scala.collection.mutable.ListBuffer


object Listt7 {
  //task 1
  def eachNElement[A](lazyList: LazyList[A], n: Int, m: Int): LazyList[A] = {
    @tailrec
    def innerLoop(lL: LazyList[A], i: Int, listIter: Int, acc: LazyList[A]): LazyList[A] =
      if listIter == m then acc
      else if i == n then innerLoop(lL.tail, 1 , listIter + 1, lL.head#::acc)
      else innerLoop(lL.tail, i + 1, listIter + 1, acc)
    if n <= 0 || m <= 0 then throw new Exception("Error in 'eachNElement': n or m is less than 1")
    innerLoop(lazyList, n, 0, LazyList()).reverse
  }

  //task 2
  def lazyExecute(lList1: LazyList[Int], lList2: LazyList[Int], operator: Char): LazyList[Int] = {
    @tailrec
    def innerLoop(lL1: LazyList[Int], lL2: LazyList[Int], acc: LazyList[Int]):LazyList[Int] =
      (lL1, lL2) match
        case (LazyList(), LazyList()) => acc
        case (LazyList(), h2#::t2) => innerLoop(lL1, t2, h2#::acc)
        case (h1#::t1, LazyList()) => innerLoop(t1, lL2, h1#::acc)
        case (h1#::t1, h2#::t2) =>
          operator match
            case '+' => innerLoop(t1, t2, (h1 + h2)#::acc)
            case '-' => innerLoop(t1, t2, (h1 - h2)#::acc)
            case '*' => innerLoop(t1, t2, (h1 * h2)#::acc)
            case '/' =>
              if h2 == 0 then throw new Exception("Error in lazyExecute.innerloop: division by 0 (element in lazyList2 is equal to 0)")
              innerLoop(t1, t2, (h1 / h2)#::acc)
    if operator != '+' && operator != '-' && operator != '+' && operator != '/' then throw new Exception("Error in 'lazyExecute': Unknown operator")
    innerLoop(lList1, lList2, LazyList()).reverse
  }

  //task 3
  def duplicate[A](seq1: Seq[A], seq2: Seq[Int]): LazyList[A] = {
    @tailrec
    def innerloop(s1: Seq[A], s2head: Int ,s2: Seq[Int], acc: LazyList[A]): LazyList[A] =
      (s1, s2) match
        case (h1+:t1, h2+:t2) =>
          if s2head <= 0 then innerloop(t1, h2, t2, acc)
          else innerloop(s1, s2head - 1, s2, h1#::acc)
        case _ => acc
    if seq2.isEmpty then LazyList()
    else innerloop(seq1, seq2.head, seq2.tail, LazyList()).reverse
  }

  //task 4 and 5
  trait Debug {
    def debugName(): String =
      this.getClass.getName

    def debugVars(): List[(String, String, String)] =
      var fields = this.getClass.getDeclaredFields
      for(x <- fields)
        x.setAccessible(true)

      var listBuffer = new ListBuffer[(String, String, String)]()

      for(x <- fields)
        listBuffer += Tuple3(x.getName, x.getType.getSimpleName, x.get(this).toString)

      listBuffer.toList
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit = {
    println(eachNElement(LazyList(5,6,3,2,1), 2, 3).toList)
    println(eachNElement(LazyList(5,6,3,2,1), 2, 4).toList)
    println(lazyExecute(LazyList(1,2,3), LazyList(2,3,4,5), '+').toList)
    println(lazyExecute(LazyList(4,6,8), LazyList(1,3,2), '/').toList)
    println(duplicate(String("HEllo"), Vector(0,3,1,4)).toList)

    var P: Point = new Point(1,2)
    println(P.debugName())
    println(P.debugVars())
  }

}
