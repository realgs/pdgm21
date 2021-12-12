import java.lang.reflect.Field

object Main {
  def eachNElement[A](lazyList: LazyList[A], n: Int, m: Int): List[A]=
    def jumper(currentPos: Int): List[A]=
      lazyList(currentPos) :: (if currentPos+n >= m then Nil else jumper(currentPos+n))

    if n > 0 && m>=0 then
      if lazyList == LazyList() then List()
      else jumper(0)
    else
      throw new Exception("Invalid n,m values")

  def lazyExecute(firstLazy: LazyList[Int], secondLazy: LazyList[Int], operation: Char): LazyList[Int] =
    (firstLazy, secondLazy) match
      case (hF#::tF, hS#::tS) =>
        operation match
          case '+' => (hF+hS) #:: lazyExecute(tF, tS, operation)
          case '-' => (hF-hS) #:: lazyExecute(tF, tS, operation)
          case '*' => (hF*hS) #:: lazyExecute(tF, tS, operation)
          case '/' => (if hF !=0 then (hF/hS) else hS) #:: lazyExecute(tF, tS, operation)
          case _ => throw new  Exception("Invalid operator")
      case (_, _) => LazyList()


  def duplicate(duplicated: List[Int], multipliers: List[Int]): List[Int] =
    (duplicated, multipliers) match
      case (hD::tD, hM::tM) => if hM > 0 then hD :: duplicate(duplicated, hM-1 :: tM) else duplicate(tD, tM)
      case (_, _) => Nil

  def main(args: Array[String]): Unit=
    println("LISTA 6")

    println("\nTesty zad 1")
    val ll1 = LazyList(5,6,3,2,1)
    println(eachNElement(ll1, 2, 3))
    println(eachNElement(ll1, 2, 4))

    println("\nTesty zad 2")
    val ll2 = LazyList(1,2,3,4,5,6,0)
    val ll3 = LazyList(1,2,3,4,5,7,0)
    println(lazyExecute(ll2, ll3, '+').toList)
    println(lazyExecute(ll2, ll3, '-').toList)
    println(lazyExecute(ll2, ll3, '*').toList)
    println(lazyExecute(ll2, ll3, '/').toList)

    println("\nTesty zad 3")
    val l1 = List(1,2,3,4)
    val l2 = List(0,3,1,4)
    println(duplicate(l1,l2))


    println("\nTesty zad 4")
    val p = Point(3,4)
    println(p.debugName)

    println("\nTesty zad 4")
    println(p.debugVars)
}

trait Debug {
  def debugName: String =
    getClass.getName

  def debugVars=
    def inner(fields: List[Field]): List[List[Any]]=
      fields match
        case h::t => h.setAccessible(true);
          List(h.getName,h.getType,h.get(this)) :: inner(t)
        case _ => List()
    inner(getClass.getDeclaredFields.toList)
}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}
