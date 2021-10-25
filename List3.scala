object List3 {


  def main(args: Array[String]): Unit = {

    def append[A] (list: List[A], elem: A): List[A] =
      if list == Nil then List(elem)
      else list.head :: append(list.tail, elem)

    def marge[A] (list1: List[A], list2: List[A]): List[A] =
      if list1 == Nil then list2
      else list1.head :: marge(list1.tail, list2)

    def splitBySign (list: List[Int]): (List[Int],List[Int]) =
      def splitHlp(list: List[Int], neg: List[Int], pos: List[Int]): (List[Int], List[Int]) =
        if list == Nil then (neg, pos)
        else if list.head < 0 then splitHlp(list.tail, append(neg, list.head), pos)
        else if list.head % 2 != 0 then splitHlp(list.tail, neg, append(pos ,list.head))
        else splitHlp(list.tail, neg, pos)
      splitHlp(list,List(),List())

    println(splitBySign(List(-1,1,-3,3,-5,9,7)));

    def listLength[A] (list: List[A]): Int =
      def listLengthIter (list: List[A], length: Int): Int =
        if list == Nil then length
        else listLengthIter(list.tail, length+1)
      listLengthIter(list,0)

    println(listLength(List(1,1,1,1,1,1,1,1)));
    println(listLength(List()));
    println(listLength(List('a','a','a')));

    def conectLists[A] (list1: List[A], list2: List[A]): List[A] =
      def conectListsHlp (list1: List[A], list2: List[A], res: List[A]): List[A] =
        if list1 == Nil && list2 == Nil then res
        else if list1 == Nil then marge(res, list2)
        else if list2 == Nil then marge(res, list1)
        else conectListsHlp(list1.tail, list2.tail, append(append(res, list1.head), list2.head))
      conectListsHlp(list1, list2, List())

    println(conectLists(List(1,3,5,7,9),List(2,4,6,8,0,8,6,4)));

  }
}
