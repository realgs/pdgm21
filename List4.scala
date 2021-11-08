object List4 {

  import scala.annotation.tailrec

  def reverse[A](xs: List[A]): List[A] =
    def reverseTail[A](xs: List[A], result: List[A]): List[A] =
      if xs == Nil then result
      else reverseTail(xs.tail, xs.head::result)
    reverseTail(xs, Nil)


  // task 2

  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2) match
      case(h1::t1,_) => h1::joinLists(t1,list2,list3)
      case(Nil,h2::t2) => h2::joinLists(list1,t2,list3)
      case(Nil,Nil) => list3


  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    @tailrec
    def joinListsRec[A](list1: List[A], list2: List[A], result: List[A]): List[A] =
      (list1, list2) match
        case(_,h2::t2) => joinListsRec(list1,t2,h2::result)
        case(h1::t1,Nil) => joinListsRec(t1,list2,h1::result)
        case(Nil,Nil) => result
    joinListsRec(reverse(list1),reverse(list2),list3)


  // task 1

  def contains(elementFromList: String, element: String): Boolean =
    @tailrec
    def containsRec(position: Int, startPosition: Int): Boolean =
      if startPosition + element.length > elementFromList.length then false
      else if element.length <= position then true
      else if elementFromList.charAt(startPosition+position) == element.charAt(position) then containsRec(position+1, startPosition)
      else containsRec(0,startPosition+1)
    containsRec(0,0)


  def find(list: List[String], elementList: List[String]): List[String] =
    def findRec(list: List[String], elementList1: List[String]): List[String] =
      (list, elementList1) match
        case (h1::t1, h2::t2) => if contains(h1,h2) then h1::findRec(t1,elementList) else findRec(list,t2)
        case (h1::t1, Nil) => findRec(t1,elementList)
        case (Nil,_) => Nil
    findRec(list,elementList)


  def findTail(list: List[String], elementList: List[String]): List[String] =
    @tailrec
    def findRec(list: List[String], elementList1: List[String], result: List[String]): List[String] =
      (list, elementList1) match
        case (h1::t1, h2::t2) => if contains(h1,h2) then findRec(t1,elementList,h1::result) else findRec(list,t2,result)
        case (h1::t1, Nil) => findRec(t1,elementList,result)
        case (Nil,_) => reverse(result)
    findRec(list,elementList,List())



  def main(args: Array[String]): Unit = {

    // joinLists tests
    println("Testy joinLists:")
    println(joinLists(List(5,4,3,2),List(1,0),List(9)))
    println(joinLists(List(5,4,3,2),List(1,0),List(9,10,11)))
    println(joinLists(List(),List(1,0),List(9)))
    println(joinLists(List(5,4,3,2),List(1,0),List()))
    println(joinLists(List(5,4,3,2),List(),List(9)))
    println(joinLists(List(),List(),List()))
    println()


    // joinListsTail tests
    println("Testy joinListsTail:")
    println(joinListsTail(List(5,4,3,2),List(1,0),List(9)))
    println(joinListsTail(List(5,4,3,2),List(1,0),List(9,10,11)))
    println(joinListsTail(List(),List(1,0),List(9)))
    println(joinListsTail(List(5,4,3,2),List(1,0),List()))
    println(joinListsTail(List(5,4,3,2),List(),List(9)))
    println(joinListsTail(List(),List(),List()))
    println()


    // find tests
    println("Testy find:")
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168")))
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168","222")))
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168","i")))
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List()))
    println(find(List(),List("index0168","222")))
    println()


    // findTail tests
    println("Testy findTail:")
    println(findTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168")))
    println(findTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168","222")))
    println(findTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168","i")))
    println(findTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List()))
    println(findTail(List(),List("index0168","222")))
    println()


  }


}
