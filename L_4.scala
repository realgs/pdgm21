import scala.annotation.tailrec

object L_4 {
  //custom list.reverse function:
  def listReverse[A](list: List[A]): List[A] = {
    @tailrec
    def listReverse_iter(list: List[A], reversed: List[A]): List[A] =
      (list) match
        case (h::t) => listReverse_iter(t, h::reversed)
        case _ => reversed
    listReverse_iter(list, Nil)
  }

  //task 1:
  //tail recursive:
  def find(data: List[String], querries: List[String]): List[String] = {
    //checks if 'substr' is a substring of 'str'
    def isSubstring(str:String, substr:String): Boolean = {
      @tailrec
      //finds a list of indexes of substr[0] characters in str
      def pre(i: Int, list: List[Int]): List[Int] =
        if i == str.length then list
        else if str.charAt(i) == substr.charAt(0) then pre(i + 1, i :: list)
        else pre(i + 1, list)

      @tailrec
      //checks potential substrings using 'pre' List
      def check(i: Int, j: Int, toCheck: List[Int]): Boolean =
        if i == -1 then
          if toCheck == Nil then false
          else check(toCheck.head, 0, toCheck.tail)
        else if j == substr.length then true
        else if i == str.length then false
        else if str.length - i < substr.length - j then false
        else if str.charAt(i) == substr.charAt(j) then check(i + 1, j + 1, toCheck)
        else if toCheck != Nil then check(toCheck.head, 0, toCheck.tail)
        else false

      if str == "" then false
      else if substr == "" then true
      else check(-1, 0, listReverse(pre(0, Nil)))
    }

    @tailrec
    //iterate over querries (checks whether any querries_element is a substr of 'data_element')
    def querry_iter(querries_copy: List[String], data_element: String): Boolean = {
      querries_copy match
        case (h::t) if isSubstring(data_element, h) => true
        case (h::t) => querry_iter(t, data_element)
        case _ => false
    }

    @tailrec
    //iterate over data (finds data_elements to return)
    def find_data_iter(data_copy: List[String], result: List[String]): List[String] = {
      data_copy match
        case (h::t) if querry_iter(querries, h) => find_data_iter(t, h::result)
        case (h::t) => find_data_iter(t, result)
        case _ => result
    }

    listReverse(find_data_iter(data, Nil))
  }

  //fully recursive
  def find_rek(data: List[String], querries: List[String]): List[String] = {
    def isSubstring(str:String, substr:String, i: Int, j: Int): Boolean =
      if j == substr.length then true
      else if i == str.length then false
      else if str.charAt(i) == substr.charAt(j) then
        if j != 0 then isSubstring(str, substr, i + 1, j + 1)
        else isSubstring(str, substr, i + 1, j + 1) || isSubstring(str, substr, i + 1, 0)
      else 
        if j > 0 then false;
        else isSubstring(str, substr, i + 1, 0)


    def querry_rek(querries_copy: List[String], data_element: String): Boolean =
      querries_copy match
        case (h::t) =>  querry_rek(t, data_element) || isSubstring(data_element, h, 0, 0)
        case _ => false

    def data_rek(data_copy: List[String]): List[String] =
      data_copy match
        case (h::t) if querry_rek(querries, h) => h::data_rek(t)
        case (h::t) => data_rek(t)
        case _ => Nil

    data_rek(data)
  }

  //task 2:
  //recursive:
  def joinLists_rek[A](l1: List[A], l2: List[A], l3: List[A]): List[A] = {
    (l1, l2, l3) match
      case (h1::t1, _, _) => h1::joinLists_rek(t1, l2, l3)
      case (_, h2::t2, _) => h2::joinLists_rek(l1, t2, l3)
      case _ => l3
  }

  //tail recursive:
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
    @tailrec
    def joinLists_iter(l1: List[A], l2: List[A], returnvalue: List[A]): List[A] = {
      (l1, l2) match
        case (_, h2::t2) => joinLists_iter(l1, t2, h2::returnvalue)
        case (h1::t1, _) => joinLists_iter(t1, l2, h1::returnvalue)
        case _ => returnvalue
    }
    joinLists_iter(listReverse(list1), listReverse(list2), list3)
  }


  def main(args: Array[String]): Unit = {
    println(joinLists(List(5,4,3,2), List(1,0), List(9)))
    println(joinLists_rek(List(5,4,3,2), List(1,0), List(9)))
    println(find(
      List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),
      List("index0168")))
    println(find_rek(
      List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),
      List("index0168")))
    println(find(
      List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),
      List("Hi")))
    println(find_rek(
      List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),
      List("Hi")))
    println(find(
      List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),
      List("index0168", "iindex")))
    println(find_rek(
      List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),
      List("index0168", "iindex")))
    println(find(
      List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),
      List("")))
    println(find_rek(
      List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),
      List("")))
  }
}
