import scala.annotation.tailrec

object L6 {

  //zadanie 1
  def eachNElement[A](llist: LazyList[A], n: Int, m: Int): LazyList[A] =
    @tailrec
    def helper[A](llist: LazyList[A], nCounter: Int, mCounter: Int, acc: LazyList[A]): LazyList[A] =
      (llist, nCounter, mCounter) match
        case (_, _, 0)         => acc.reverse
        case (h #:: t, 0, mc)  => helper(t, n-1, mc-1, h #:: acc)
        case (h #:: t, nc, mc) => helper(t, nc-1, mc-1, acc)
    helper(llist, 0, m, LazyList())

  //zadanie 2
  def lazyExecute(ll1: LazyList[Int], ll2: LazyList[Int], operator: Char): LazyList[Int] =
    @tailrec
    def helper(ll1: LazyList[Int], ll2: LazyList[Int], operator: Char, acc: LazyList[Int]): LazyList[Int] =
      (ll1, ll2) match
        case (LazyList(), LazyList()) => acc.reverse
        case (LazyList(), rest)       => acc.reverse #::: rest
        case (rest, LazyList())       => acc.reverse #::: rest
        case (h1 #:: t1, h2 #:: t2)   =>
          if operator == '+' then helper(t1, t2, operator, (h1+h2) #:: acc)
          else if operator == '-' then helper(t1, t2, operator, (h1-h2) #:: acc)
          else if operator == '*' then helper(t1, t2, operator, (h1*h2) #:: acc)
          else if operator == '/' then helper(t1, t2, operator, (h1/h2) #:: acc)
          else throw new Exception("Not supported operator")
    helper(ll1, ll2, operator, LazyList())

  //zadanie 3
  def duplicate(list: LazyList[Int], dupList: LazyList[Int]): LazyList[Int] =
    @tailrec
    def helper(list: LazyList[Int], reps: LazyList[Int], acc: LazyList[Int]): LazyList[Int] =
      (list, reps) match
        case (LazyList(), _)        => acc.reverse
        case (_, LazyList())        => acc.reverse
        case (h1 #:: t1, h2 #:: t2) =>
          if h2 > 0 then helper(list, (h2-1) #:: t2, h1 #:: acc)
          else helper(t1, t2, acc)
    helper(list, dupList, LazyList())

  //zadanie 4
  trait Debug {
    def debugName(): String =
      getClass.getSimpleName

        //zadanie 5
    def debugVars(): List[Any] =
      getClass.getDeclaredFields.toList.map(f => {f.setAccessible(true); (f.getName, f.getType, f.get(this))})

  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit = {

    println(eachNElement(LazyList(5,6,3,2,1),2,3).force)
    println(eachNElement(LazyList(5,6,3,2,1),2,4).force)

    println(lazyExecute(LazyList(1,2,3), LazyList(2,3,4,5), '+').force)
    println(lazyExecute(LazyList(9,9,7), LazyList(0,8,6,6,0), '-').force)
    println(lazyExecute(LazyList(2,2,2), LazyList(2,1,0), '*').force)
    println(lazyExecute(LazyList(6,6,6,7), LazyList(3,6,2), '/').force)

    println(duplicate(LazyList(1,2,3), LazyList(0,3,1,4)).force)
    println(duplicate(LazyList(1,2,3), LazyList(0,3)).force)

    var p = new Point(3, 4)
    println(p.debugName())
    println(p.debugVars())

  }
}
