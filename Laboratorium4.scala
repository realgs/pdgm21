package Test

import Test.Lista4.joinListTail

object Lista4 {
  def main(args: Array[String]): Unit = {

    println("joinList")
    println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinLists(List(5, 4), List(1, 0, 6, 8), List(9)) == List(5, 4, 1, 0, 6, 8, 9))
    println(joinLists(List(5, 4), List(1, 0), List(9)) == List(5, 4, 1, 0, 9))
    println(joinLists(List(5, 4), List(1, 0), List(9, 6)) == List(5, 4, 1, 0, 9, 6))
    println(joinLists(List(5, 4), List(1, 0), List(9, 6, 12, 6)) == List(5, 4, 1, 0, 9, 6, 12, 6))

    println("joinListTail")
    println(joinListTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinListTail(List(5, 4), List(1, 0, 6, 8), List(9)) == List(5, 4, 1, 0, 6, 8, 9))
    println(joinListTail(List(5, 4), List(1, 0), List(9)) == List(5, 4, 1, 0, 9))
    println(joinListTail(List(5, 4), List(1, 0), List(9, 6)) == List(5, 4, 1, 0, 9, 6))
    println(joinListTail(List(5, 4), List(1, 0), List(9, 6, 12, 6)) == List(5, 4, 1, 0, 9, 6, 12, 6))

    println("Find")
    println(findTail(List("iindex0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "iindex0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(find(List("iindex0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "iindex0168") == List("iindex0168202", "iindex0168211", "iindex0168210"))
    println(findList(List("iindex0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("iindex0168", "index0169224")) == List("iindex0168202", "iindex0168211", "iindex0168210", "index0169224"))
    println(findListTail(List("iindex0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("iindex0168", "index0169224")) == List("iindex0168202", "iindex0168211", "iindex0168210", "index0169224"))

  }

  def joinLists[A](xs: List[A], ys: List[A], zs: List[A]): List[A] = {
    (xs, ys, zs) match {
      case (List(), _, List()) => ys
      case (_, List(), List()) => xs
      case (List(), List(), _) => zs
      case (List(), _, _) => ys.head :: joinLists(List(), ys.tail, zs)
      case (_, _, _) => xs.head :: joinLists(xs.tail, ys, zs)
    }
  }

  def joinListTail[A](xs: List[A], ys: List[A], zs: List[A]): List[A] = {
    def joinListHelp[A](xs: List[A], ys: List[A], zs: List[A], result: List[A]): List[A] = {
      (xs, ys, zs) match {
        case (Nil, Nil, Nil) => result
        case (Nil, Nil, _) => joinListHelp(xs, ys, zs.tail, result :+ zs.head)
        case (Nil, _, _) => joinListHelp(xs, ys.tail, zs, result :+ ys.head)
        case (_, _, _) => joinListHelp(xs.tail, ys, zs, result :+ xs.head)
      }
    }

    joinListHelp(xs, ys, zs, List())
  }


  def stringFound(findElement: String, element: String): Boolean = {
    (findElement, element) match {
      case ("", _) => true
      case (_, "") => false
      case (findElement, element) => if findElement.head == element.head then stringFound(findElement.tail, element.tail) else stringFound(findElement, element.tail)
    }
  }

  def stringFoundList(findElement: List[String], element: String): Boolean = {
    findElement match {
      case Nil => false
      case h :: t => if stringFound(h, element) then true else stringFoundList(t, element)
    }
  }

  def findTail(list: List[String], element: String): List[String] = {
    def findIn(list: List[String], element: String, result: List[String]): List[String] = {
      list match {
        case Nil => result
        case h :: t => if stringFound(element, h) then findIn(t, element, h :: result) else findIn(t, element, result)
      }
    }

    reverse(findIn(list, element, Nil))
  }



  def find(list: List[String], element: String): List[String] = {
    list match {
      case Nil => Nil
      case h :: t => if stringFound(element, h) then h :: find(t, element) else find(t, element)
    }
  }


  def findList(list: List[String], element: List[String]): List[String] = {
    (list, element) match {
      case (Nil, _) => Nil
      case (_, Nil) => list
      case (hl :: tl, he :: te) => if stringFoundList(element, hl) then hl :: findList(tl, element) else findList(tl, element)
    }
  }



  def findListTail(list: List[String], element: List[String]): List[String] = {
    def findIn(list: List[String], element: List[String], result: List[String]): List[String] = {
      (list, element) match {
        case (Nil, _) => result
        case (_, Nil) => list
        case (hl :: tl, he :: te) => if stringFoundList(element, hl) then findIn(tl, element, hl :: result) else findIn(tl, element, result)
      }
    }

    reverse(findIn(list, element, Nil))
  }

  def reverse (list: List[String]): List[String] ={
    def reveseIn(list: List[String], result: List[String]): List[String] = {
      list match {
        case Nil => result
        case h::t => reveseIn(t,h::result)
      }
    }
    reveseIn(list, Nil)
  }

}
