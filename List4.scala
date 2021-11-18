import annotation.tailrec

object List4 {

  //własne contains
  def contains(string: String, substring: String): Boolean =
    (string, substring) match {
      case (_, "") => true
      case ("", _) => false
      case (_, _) => if string.head == substring.head then contains(string.tail, substring.tail)
                     else contains(string.tail, substring)
    }

  //własne reverse
  def reverse[A](list: List[A]): List[A] =
    @tailrec
    def reverseIn[A](list: List[A], revList: List[A]): List[A] =
      list match
        case Nil => revList
        case head :: tail => reverseIn(list.tail, head :: revList)
    reverseIn(list, Nil)


  //Zadanie 1
  def find(list: List[String], n: String): List[String] =
    list match {
      case Nil => Nil
      case _ => if contains(list.head, n) then list.head :: find(list.tail, n)
                else find(list.tail, n)
    }

  def find2(list: List[String], n: String): List[String] =
    @tailrec
    def find2In(list: List[String], n: String, result: List[String]): List[String] =
      list match
        case Nil => result
        case _ => if contains(list.head, n) then find2In(list.tail, n, list.head :: result)
                  else find2In(list.tail, n, result)

    reverse(find2In(list, n, Nil))


  def containsList(string: String, list: List[String]): Boolean =
    list match
      case Nil => false
      case _ => if contains(string, list.head) then true
                else containsList(string, list.tail)

  def findN(list: List[String], n: List[String]): List[String] =
    list match
      case Nil => Nil
      case _ => if containsList(list.head, n) then list.head :: findN(list.tail, n)
                else findN(list.tail, n)


  def findN2(list: List[String], n: List[String]): List[String] =
    @tailrec
    def findN2In(list: List[String], n: List[String], result: List[String]): List[String] =
      (list, n) match
        case (Nil, _) => result
        case (_, Nil) => list
        case (_, _) => if containsList(list.head, n) then findN2In(list.tail, n, list.head :: result)
                       else findN2In(list.tail, n, result)

    reverse(findN2In(list, n, Nil))


  //Zadanie 2
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (head1 :: tail1, _, _) => head1 :: joinLists(tail1, list2, list3)
      case (Nil, head2 :: tail2, _) => head2 :: joinLists(Nil, tail2, list3)
      case (Nil, Nil, head3 :: tail3) => head3 :: joinLists(Nil, Nil, tail3)
      case (Nil, Nil, Nil) => Nil


  def joinLists2[A](list1: List[A], list2: List[A], list3: List[A]) =
    @tailrec
    def joinLists2In[A](list1: List[A], list2: List[A], list3: List[A], result: List[A]): List[A] =
      (list1, list2, list3) match
        case (head1 :: tail1, _, _) => joinLists2In(tail1, list2, list3, head1 :: result)
        case (Nil, head2 :: tail2, _) => joinLists2In(Nil, tail2, list3, head2 :: result)
        case (Nil, Nil, head3 :: tail3) => joinLists2In(Nil, Nil, tail3, head3 :: result)
        case (Nil, Nil, Nil) => reverse(result)
    joinLists2In(list1, list2, list3, Nil)


  def main(args: Array[String]): Unit = {

    System.out.println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9))==List(5, 4, 3, 2, 1, 0, 9))
    System.out.println(joinLists2(List(5, 4, 3, 2), List(1, 0), List(9))==List(5, 4, 3, 2, 1, 0, 9))
    System.out.println(joinLists(List(1,2,3), List(), List())==List(1,2,3))
    System.out.println(joinLists2(List(1,2,3), List(), List())==List(1,2,3))
    System.out.println(joinLists(List(), List(1,2,3), List())==List(1,2,3))
    System.out.println(joinLists2(List(), List(1,2,3), List())==List(1,2,3))
    System.out.println(joinLists(List(), List(), List(1,2,3))==List(1,2,3))
    System.out.println(joinLists2(List(), List(), List(1,2,3))==List(1,2,3))
    System.out.println(joinLists(List(), List(), List())==List())
    System.out.println(joinLists2(List(), List(), List())==List())



    System.out.println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    System.out.println(find2(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    System.out.println(find(List(),"nic")==List())
    System.out.println(find2(List(),"nic")==List())
    System.out.println(find(List("ala","ma","kota"),"a")==List("ala","ma","kota"))
    System.out.println(find2(List("ala","ma","kota"),"a")==List("ala","ma","kota"))



    System.out.println(findN(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("210","iin"))==List("iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"))
    System.out.println(findN2(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("210","iin"))==List("iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"))

  }
}


