import scala.annotation.tailrec

object l6 {
  // Mnoika Jung L6
  // Task 1 --------------------------------------------------------------------------------------------------------------
  @tailrec
  def takeNthElemOfLazyList[A](llist:LazyList[A], index:Int):(A,LazyList[A]) =
    if index < 0 then throw new Exception("wrong index!")
    else if index == 0 then (llist.head, llist.tail)
    else takeNthElemOfLazyList(llist.tail, index-1)


  def eachNthElemS[A] (llist:Stream[A], eachN:Int, lastIndex:Int):Stream[A] =
    if lastIndex < 0 then throw new Exception("last index can't be smaller than 0!")
    else if eachN <= 0 then throw new Exception("N can't be smaller than 1!")
    else
      @tailrec
      def recEachNthElem[A] (llistT:Stream[A], current:Int,  result:Stream[A]):Stream[A] =
        (llistT, current, lastIndex) match
          case (Stream(), _, _)                     => result
          case (_,_,0)                              => result
          case (head #:: tail, current, lastIndex)  => if current >= lastIndex then result
            else if current == 0 || current%eachN == 0
             then recEachNthElem(tail, current + 1, result#:::Stream(head))
            else recEachNthElem(tail, current + 1, result)
      recEachNthElem(llist,0, Stream())

  def eachNthElem[A] (llist:LazyList[A], eachN:Int, lastIndex:Int):LazyList[A] =
    if lastIndex < 0 then throw new Exception("last index can't be smaller than 0!")
    else if eachN <= 0 then throw new Exception("N can't be smaller than 1!")
    else
      @tailrec
      def recEachNthElem[A] (llistT:LazyList[A], current:Int,  result:LazyList[A]):LazyList[A] =
        (llistT, current, lastIndex) match
          case (LazyList(), _, _)                   => result
          case (_,_,0)                              => result
          case (head #:: tail, current, lastIndex)  => if current >= lastIndex then result
            else if current == 0 || current%eachN == 0
              then recEachNthElem(tail, current + 1, result#:::LazyList(head))
            else recEachNthElem(tail, current + 1, result)
      recEachNthElem(llist,0, LazyList())


  def eachNthElemAternative[A](llist:LazyList[A], eachN:Int, lastIndex:Int):List[A] =
    if lastIndex < 0 then throw new Exception("last index can't be smaller than 0!")
    else if eachN <= 0 then throw new Exception("N can't be smaller than 1!")
    else
      @tailrec
      def recEachNthElem[A] (llistT:LazyList[A], current:Int, result:List[A]):List[A] =
      //println("start: " +result)
        if current + eachN > lastIndex then {
          //println("current: "+current+" >= last: " + lastIndex)
          result
        }
        else if current == 0 then recEachNthElem(llistT.tail, current+1, List(llistT.head))
        else {
          var pair: (A, LazyList[A]) = takeNthElemOfLazyList(llistT, eachN-1)
          //println("current: " + current + " < last: " + lastIndex)
          //println("pair: " + pair._1)
          //println(result)
          recEachNthElem(pair._2, current + eachN, result ::: List(pair._1))
        }
      recEachNthElem(llist,0, Nil)

  // Task 2 --------------------------------------------------------------------------------------------------------------
  def addLazyListsElements (llistA:LazyList[Int], llistB:LazyList[Int]):LazyList[Int] =
    @tailrec
    def recAddLazyListsElem (llistA:LazyList[Int], llistB:LazyList[Int], result:LazyList[Int]):LazyList[Int] =
      (llistA,llistB) match
        case (LazyList(),_) => result#:::llistB
        case (_,LazyList()) => result#:::llistA
        case (headA#::tailA, headB#::tailB) => recAddLazyListsElem(tailA,tailB,result#:::LazyList(headA+headB))
    recAddLazyListsElem(llistA,llistB,LazyList())

  def subtractLazyListsElements (llistA:LazyList[Int], llistB:LazyList[Int]):LazyList[Int] =
    @tailrec
    def recSubtractLazyListsElem (llistA:LazyList[Int], llistB:LazyList[Int], result:LazyList[Int]):LazyList[Int] =
      (llistA,llistB) match
        case (LazyList(),_) => result#:::llistB
        case (_,LazyList()) => result#:::llistA
        case (headA#::tailA, headB#::tailB) => recSubtractLazyListsElem(tailA,tailB,result#:::LazyList(headA-headB))
    recSubtractLazyListsElem(llistA,llistB,LazyList())

  def multiplyLazyListsElements (llistA:LazyList[Int], llistB:LazyList[Int]):LazyList[Int] =
    @tailrec
    def recMultiplyLazyListsElem (llistA:LazyList[Int], llistB:LazyList[Int], result:LazyList[Int]):LazyList[Int] =
      (llistA,llistB) match
        case (LazyList(),_) => result#:::llistB
        case (_,LazyList()) => result#:::llistA
        case (headA#::tailA, headB#::tailB) => recMultiplyLazyListsElem(tailA,tailB,result#:::LazyList(headA*headB))
    recMultiplyLazyListsElem(llistA,llistB,LazyList())

  def divideLazyListsElements (llistA:LazyList[Int], llistB:LazyList[Int]):LazyList[Int] =
    @tailrec
    def recDivideLazyListsElem (llistA:LazyList[Int], llistB:LazyList[Int], result:LazyList[Int]):LazyList[Int] =
      (llistA,llistB) match
        case (LazyList(),_) => result#:::llistB
        case (_,LazyList()) => result#:::llistA
        case (headA#::tailA, headB#::tailB) => recDivideLazyListsElem(tailA,tailB,result#:::LazyList(headA/headB))
    recDivideLazyListsElem(llistA,llistB,LazyList())

  //we're going to save extra elements
  def lazyExecute (llistA:LazyList[Int], llistB:LazyList[Int], sign:String):LazyList[Int] =
    sign match {
      case "+" => addLazyListsElements(llistA,llistB)
      case "-" => subtractLazyListsElements(llistA,llistB)
      case "*" => multiplyLazyListsElements(llistA,llistB)
      case "/" => divideLazyListsElements(llistA,llistB)
      case _ => throw new Exception("Wrong sign!!!")
    }

  // Task 3 --------------------------------------------------------------------------------------------------------------
  @tailrec
  def copyOneLazyListElem[A] (result:LazyList[A], elem:A, copyNr:Int):LazyList[A] =
    if copyNr <= 0 then result
    else copyOneLazyListElem(result #:::LazyList(elem), elem, copyNr - 1)

  def copyLazyListElem[A] (llistA:LazyList[A], llistB:LazyList[Int]):LazyList[A] =
    @tailrec
    def recCopyLazyListElem[A] (llistA:LazyList[A], llistB:LazyList[Int], result:LazyList[A]):LazyList[A] =
      (llistA,llistB) match
        case (LazyList(),_) => result
        case (_,LazyList()) => result
        case (headA#::tailA, headB#::tailB) =>
          recCopyLazyListElem(tailA,tailB,copyOneLazyListElem(result,headA,headB))
    recCopyLazyListElem(llistA,llistB,LazyList())

  // Task 4 & 5 ----------------------------------------------------------------------------------------------------------

  trait Debug {
    def debugClassName(): String = getClass.getName()

    def debugClassVars(): List[List[Object]] =
      getClass.getDeclaredFields.toList.map {
        fun => fun.setAccessible(true)
          val result = List(fun.getName(), fun.getType(), fun.get(this))
          fun.setAccessible(false)
          result
      }
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]):Unit = {

    var llist1: LazyList[Int] = LazyList()
    llist1 = 1#::llist1.map(n=>n*2)
    //val llist1: LazyList[Int] = LazyList(<not computed>)
    var stream1: Stream[Int] = Stream()
    stream1 = 1#::stream1.map(n=>n*2)
    //val stream1: Stream[Int] = Stream(1, <not computed>)
    var llist2: LazyList[Int] = LazyList()
    llist2 =  1#::llist2.map(n=>n+1)
    //val llist1: LazyList[Int] = LazyList(<not computed>)
    var stream2: Stream[Int] = Stream()
    stream2 = 1#::stream2.map(n=>n+1)
    //val stream1: Stream[Int] = Stream(1, <not computed>)

    println("LazyLists and Streams")
    println(llist1.take(6).toList == List(1, 2, 4, 8, 16, 32))
    println(stream1.take(6).toList == List(1, 2, 4, 8, 16, 32))
    println(llist2.take(6).toList == List(1, 2, 3, 4, 5, 6))
    println(stream2.take(6).toList == List(1, 2, 3, 4, 5, 6))

    //task 1 test
    println("TASK 1 TEST")
    println(takeNthElemOfLazyList(llist1,1)._1 == 2)
    println(takeNthElemOfLazyList(llist1,2)._1 == 4)
    println(takeNthElemOfLazyList(llist1,3)._1 == 8)
    println(takeNthElemOfLazyList(llist1,4)._1 == 16)

    println(eachNthElemS(stream1,1,10).toList == List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512))
    println(eachNthElemS(stream1,3,7).toList == List(1, 8, 64))
    println(eachNthElemS(stream1,3,8).toList == List(1, 8, 64))
    println(eachNthElemS(stream1,3,10).toList == List(1, 8, 64, 512))

    println(eachNthElem(llist1, 1, 10).toList == List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512))
    println(eachNthElem(llist1, 3, 7).toList == List(1, 8, 64))
    println(eachNthElem(llist1, 3, 8).toList == List(1, 8, 64))
    println(eachNthElem(llist1, 3, 10).toList == List(1, 8, 64, 512))

    println(eachNthElemAternative(llist1,1,10) == List(1, 2, 4, 8, 16, 32, 64, 128, 256, 512))
    println(eachNthElemAternative(llist1, 3, 7) == List(1, 8, 64))
    println(eachNthElemAternative(llist1, 3, 8) == List(1, 8, 64))
    println(eachNthElemAternative(llist1,3,10) == List(1, 8, 64, 512))

    //task 2 test
    println("TASK 2 TEST")
    val llist01 = LazyList(7,8,9,20,30)
    val llist02 = LazyList(3,2,1)

    println(lazyExecute(llist01,llist02, "+").toList == List(10, 10, 10, 20, 30))
    println(lazyExecute(llist01,llist02, "-").toList == List(4, 6, 8, 20, 30))
    println(lazyExecute(llist01,llist02, "*").toList == List(21, 16, 9, 20, 30))
    println(lazyExecute(llist01,llist02, "/").toList == List(2, 4, 9, 20, 30))

    //task 3 test
    println("TASK 3 TEST")
    val llist001 = LazyList(0,1,2,3,4,5)
    val llist002 = LazyList(3,2,0,1,2)

    println(copyLazyListElem(llist001,llist002).toList == List(0, 0, 0, 1, 1, 3, 4, 4))

    //task 4 & 5 test
    println("TASK 4 & 5 TEST")
    var p: Point = new Point (3,4)

    println(p.debugClassName())
    //val res25: String = Point
    println(p.debugClassVars())
    //val res26: List[List[Object]] = List(List(x, int, 3), List(y, int, 4), List(a, class java.lang.String, test))
  }
}
