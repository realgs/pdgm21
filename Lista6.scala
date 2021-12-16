object Lista6
{
  import scala.annotation.tailrec
  //Zad. 1

  def eachNElement[A](lazyList: LazyList[A], sive: Int, length: Int): LazyList[A] =
    @tailrec
    def eachNElementHelper[A](lazyList: LazyList[A], counter: Int, length: Int, result:LazyList[A]): LazyList[A] =
      if length == 0 || lazyList == LazyList() then result
      else
        lazyList match
         case hd #:: tl =>
           if counter == 1 then eachNElementHelper(tl, sive, length - 1, hd#::result)
           else  eachNElementHelper(tl, counter - 1, length - 1, result)

    if sive < 1 then LazyList()
    else
      eachNElementHelper(lazyList, 1, length, LazyList()).reverse

  //Zad. 2

  def lazyExecute(lazyList1: LazyList[Int], lazyList2: LazyList[Int], operation: String): LazyList[Int] =
    def lazyExecuteAdd(lazyList1: LazyList[Int], lazyList2: LazyList[Int]): LazyList[Int] =
      (lazyList1, lazyList2) match
        case (hd1#::tl1, hd2#::tl2) =>  (hd1 + hd2)#::lazyExecuteAdd(tl1, tl2)
        case (_, hd2#::tl2) =>  lazyList2
        case (hd1#::tl1, _) =>  lazyList1
        case _ => LazyList()

    def lazyExecuteSub(lazyList1: LazyList[Int], lazyList2: LazyList[Int]): LazyList[Int] =
     (lazyList1, lazyList2) match
       case (hd1#::tl1, hd2#::tl2) =>  (hd1 - hd2)#::lazyExecuteSub(tl1, tl2)
       case (_, hd2#::tl2) =>  lazyList2
       case (hd1#::tl1, _) =>  lazyList1
       case _ => LazyList()

    def lazyExecuteMul(lazyList1: LazyList[Int], lazyList2: LazyList[Int]): LazyList[Int] =
       (lazyList1, lazyList2) match
        case (hd1#::tl1, hd2#::tl2) =>  (hd1 * hd2)#::lazyExecuteMul(tl1, tl2)
        case (_, hd2#::tl2) =>  lazyList2
        case (hd1#::tl1, _) =>  lazyList1
        case _ => LazyList()

    def lazyExecuteDiv(lazyList1: LazyList[Int], lazyList2: LazyList[Int]): LazyList[Int] =
     (lazyList1, lazyList2) match
       case (hd1#::tl1, hd2#::tl2) =>
         if hd2 != 0 then (hd1 / hd2)#::lazyExecuteDiv(tl1, tl2)
         else lazyExecuteDiv(tl1, tl2)
       case (_, hd2#::tl2) =>  lazyList2
       case (hd1#::tl1, _) =>  lazyList1
       case _ => LazyList()

    operation match
     case "+" => lazyExecuteAdd(lazyList1, lazyList2)
     case "-" => lazyExecuteSub(lazyList1, lazyList2)
     case "*" => lazyExecuteMul(lazyList1, lazyList2)
     case "/" => lazyExecuteDiv(lazyList1, lazyList2)
     case _ => throw new Exception("Nieobsługiwany operator")

  //Zad. 3

  def duplicate[A](lazyList1: LazyList[A], lazyList2: LazyList[Int]): LazyList[A] =
    def duplicateHelper(obj: A, counter: Int): LazyList[A] =
      if counter <= 0 then LazyList()
      else obj#::duplicateHelper(obj, counter - 1 )

    (lazyList1, lazyList2) match
      case (hd1#::tl1, hd2#::tl2) =>  duplicateHelper(hd1, hd2)#:::duplicate(tl1, tl2)
      case _ =>  LazyList()

 //Zad. 4

  trait Debug
  {
    def debugName(): String =  this.getClass().getSimpleName()

 //Zad. 5

    def debugVars():List[(String, java.lang.reflect.AnnotatedType, Object)]=
      this.getClass().getDeclaredFields().map(x =>
        x.setAccessible(true)
        (x.getName(), x.getAnnotatedType(), x.get(this))).toList
  }

  class Point(xv: Int, yv: Int) extends Debug
  {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
    //val s:(x: Int) => Int = x => x
  }

  def main(args: Array[String]): Unit =

    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 2, 8).toList)
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 0, 8).toList)
    println(eachNElement(LazyList(), 5, 8).toList)
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 11, 8).toList)
    println()

    println(lazyExecute(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), LazyList(1, 6, 7, 8, 9, 10), "+").toList)
    println(lazyExecute(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), LazyList(1, 2, 0, 2, 4, 2, 0), "/").toList)
    println()

    println(duplicate(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), LazyList(1, 2, 0, 2, 4, 2, 0)).toList)
    println(duplicate(LazyList("a", "b", "c", "d"), LazyList(1, 2, 3, 4)).toList)
    println(duplicate(LazyList("a", "b", "c", "d"), LazyList()).toList)
    println(duplicate(LazyList(), LazyList(20)).toList)
    println()

    var p:Point = new Point(2,3)
    println(p.debugName())
    println(p.debugVars())
}
