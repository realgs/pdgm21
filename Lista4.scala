//Szymon Sawczuk

import scala.annotation.tailrec

object Lista4 {

//Helping functions
  def reverseList[A](list:List[A]):List[A] =
    def reverseListIter(list:List[A], reversed:List[A]):List[A] =
      if list == Nil then reversed
      else reverseListIter(list.tail, list.head::reversed)

    reverseListIter(list, Nil)

// Zadanie 1
  def doesContain(text:String, pattern:String) =
    def search(text:String, pattern_helper:String, result:Boolean):Boolean =
      (text, pattern_helper) match
        case (_, "") => result
        case ("", _) => false
        case (_, _) => if text(0) == pattern_helper(0) then search(text.substring(1), pattern_helper.substring(1), true)
          else search(if pattern_helper == pattern then text.substring(1) else text, pattern, false)
    search(text, pattern, pattern == "")

  def doesContain2(text:String, pattern:String)(originalPattern:String = pattern):Boolean =
      (text, pattern) match
        case (_, "") => true
        case ("", _) => false
        case (_, _) => if text(0) == pattern(0) then doesContain2(text.substring(1), pattern.substring(1))(originalPattern)
          else doesContain2(if pattern == originalPattern then text.substring(1) else text, originalPattern)()

  def find(list:List[String], elements:List[String]) =
    @tailrec
    def findIter(list:List[String], elements_helper:List[String], result:List[String]):List[String] =
      if list == Nil then reverseList(result)
      else if elements_helper == Nil || doesContain(list.head, elements_helper.head) then findIter(list.tail, elements, if elements_helper != Nil then list.head::result else result)
      else findIter(list, elements_helper.tail, result)
    findIter(list, elements, Nil)

  def find2(list:List[String])(elements:List[String])(originalElements:List[String] = elements):List[String] =
    if list == Nil then Nil
    else if elements == Nil then find2(list.tail)(originalElements)(originalElements)
    else if elements != Nil && doesContain(list.head, elements.head) then list.head::find2(list.tail)(originalElements)(originalElements)
    else find2(list)(elements.tail)(originalElements)



//Zadanie 2
  def jointLists [A](list1:List[A], list2:List[A], list3:List[A]) =
    def jointIter(list1:List[A], list2:List[A], list3:List[A], result:List[A]):List[A] =
      (list1, list2, list3) match
        case (Nil, Nil, Nil) => reverseList(result)
        case (Nil, head::tail, _) => jointIter(Nil, tail, list3, head::result)
        case (Nil, Nil, head::tail) => jointIter(Nil, Nil, tail, head::result)
        case (head::tail, _, _) => jointIter(tail, list2, list3, head::result)

    jointIter(list1, list2, list3, Nil)

  def jointLists2 [A](list1:List[A], list2:List[A], list3:List[A]):List[A] =
    (list1, list2, list3) match
      case (Nil, Nil, Nil) => Nil
      case (Nil, head::tail, _) => head::jointLists2(Nil, tail, list3)
      case (Nil, Nil, head::tail) => head::jointLists2(Nil, Nil, tail)
      case (head::tail, _, _) => head::jointLists2(tail, list2, list3)

  def main(args: Array[String]): Unit = {

    println(jointLists (List(5,4,3,2), List(1,0), List(9)))
    println(jointLists (Nil, List(1,0), List(9)))
    println(jointLists (List("Ala", "ma"), Nil, List("kota")))
    println(jointLists (List(5,4,3,2), List(1,0), Nil))
    println(jointLists (Nil, Nil, Nil))

    println(jointLists2 (List(5,4,3,2), List(1,0), List(9)))
    println(jointLists2 (Nil, List(1,0), List(9)))
    println(jointLists2 (List("Ala", "ma"), Nil, List("kota")))
    println(jointLists2 (List(5,4,3,2), List(1,0), Nil))
    println(jointLists2 (Nil, Nil, Nil))

    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0167222", "index0169224"), List("index0168", "index0169")))
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0167222", "index0169224"), List("index0168")))
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0167222", "index0169224"), List("")))
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0167222", "index0169224"), Nil))

    println(find2(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0167222", "index0169224"))(List("index0168", "index0169"))())
    println(find2(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0167222", "index0169224"))(List("index0168"))())
    println(find2(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0167222", "index0169224"))(List(""))())
    println(find2(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0167222", "index0169224"))(Nil)())

  }

}
