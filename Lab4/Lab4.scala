package Lab4

import scala.annotation.tailrec

object lab4 {

  //Zadanie 1

  def find_elements(list: List[String], elements: List[String]): List[String] =
    if list == Nil then Nil
    else if elements == Nil then list
    else if contain_elements(list.head, elements) then list.head :: find_elements(list.tail, elements)
    else find_elements(list.tail, elements)

  def contain_elements(segment: String, elements: List[String]): Boolean =
    if elements == Nil then false
    else if contain_element(segment, elements.head) then true
    else contain_elements(segment, elements.tail)

  def contain_element(segment: String, element: String): Boolean =
    (segment, element) match
      case (_, "") => true
      case ("", _) => false
      case (_, _) => segment_iter(segment, element) || contain_element(segment.tail, element)

  def segment_iter(segment: String, element: String): Boolean =
    (segment, element) match
      case (_, "") => true
      case ("", _) => false
      case (_, _) => if segment.head == element.head then segment_iter(segment.tail, element.tail) else false


  def find_elementsT(list: List[String], elements: List[String]): List[String] =
    @tailrec
    def find_elementsT_helper(list: List[String], elements: List[String], acc: List[String]): List[String] =
      if list == Nil then acc
      else if elements == Nil then list
      else if contain_elementsT(list.head, elements) then find_elementsT_helper(list.tail, elements, list.head :: acc)
      else find_elementsT_helper(list.tail, elements, acc)

    find_elementsT_helper(list, elements, List())

  @tailrec
  def contain_elementsT(segment: String, elements: List[String]): Boolean =
    if elements == Nil then false
    else if contain_elementT(segment, elements.head) then true
    else contain_elementsT(segment, elements.tail)

  @tailrec
  def contain_elementT(segment: String, element: String): Boolean =
    (segment, element) match
      case (_, "") => true
      case ("", _) => false
      case (_, _) => segment_iterT(segment, element) || contain_elementT(segment.tail, element)

  @tailrec
  def segment_iterT(segment: String, element: String): Boolean =
    (segment, element) match
      case (_, "") => true
      case ("", _) => false
      case (_, _) => if segment.head == element.head then segment_iterT(segment.tail, element.tail) else false



  //Zadanie 2

  def joinList[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (h1 :: t1, _, _) => h1 :: joinList(t1, list2, list3)
      case (Nil, h2 :: t2, _) => h2 :: joinList(Nil, t2, list3)
      case (Nil, Nil, _) => list3

  def reverse[A](list: List[A]): List[A] =
    @tailrec
    def helper(list: List[A], acc: List[A]): List[A] =
      list match
        case Nil => acc
        case hd :: tl => helper(tl, hd :: acc)

    helper(list, Nil)

  def joinListTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    @tailrec
    def helper[B](list1: List[B], list2: List[B], list3: List[B], acc: List[B]): List[B] =
      (list1, list2, list3) match
        case (h1 :: t1, _, _) => helper(t1, list2, list3, h1 :: acc)
        case (Nil, h2 :: t2, _) => helper(list1, t2, list3, h2 :: acc)
        case (Nil, Nil, h3 :: t3) => helper(list1, list2, t3, h3 :: acc)
        case (Nil, Nil, Nil) => acc.reverse

    helper(list1, list2, list3, List())

  def main(args: Array[String]): Unit = {


    println(find_elements(List("Robert", "Kubica", "driver", "blyskawica", "czadu", "da", "on", "za", "3"), List("ca")) == List("Kubica", "blyskawica"))
    println(find_elements(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")) == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(find_elements(List("orzel,", "orzel", "orzel", "orzel", "***********", "ty", "sie", "upieczesz", "w", "tym", "swetrze"), List()) == List("orzel,", "orzel", "orzel", "orzel", "***********", "ty", "sie", "upieczesz", "w", "tym", "swetrze"))

    println(find_elementsT(List("Robert", "Kubica", "driver", "blyskawica", "czadu", "da", "on", "za", "3"), List("ca")) == List("blyskawica", "Kubica"))
    println(find_elementsT(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")) == List("iindex0168210", "iindex0168211", "iindex0168202"))
    println(find_elementsT(List("orzel,", "orzel", "orzel", "orzel", "***********", "ty", "sie", "upieczesz", "w", "tym", "swetrze"), List()) == List("orzel,", "orzel", "orzel", "orzel", "***********", "ty", "sie", "upieczesz", "w", "tym", "swetrze"))

    println(joinList(List(2, 2, 3, 2), List(1, 2, 3, 4), List(99, 12, 12)) == List(2, 2, 3, 2, 1, 2, 3, 4, 99, 12, 12))
    println(joinList(List("Mam", "minut"), List(" 5", " 5", " 5"), List("by", "wyjsc", "i zrobic  wiecej")) == List("Mam", "minut", " 5", " 5", " 5", "by", "wyjsc", "i zrobic  wiecej"))

    println(joinListTail(List(2, 2, 3, 2), List(1, 2, 3, 4), List(99, 12, 12)) == List(2, 2, 3, 2, 1, 2, 3, 4, 99, 12, 12))
    println(joinListTail(List("Mam", "minut"), List(" 5", " 5", " 5"), List("by", "wyjsc", "i zrobic  wiecej")) == List("Mam", "minut", " 5", " 5", " 5", "by", "wyjsc", "i zrobic  wiecej"))
  }
}
