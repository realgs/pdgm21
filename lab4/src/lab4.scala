import scala.annotation.tailrec

object lab4 {

  def reverse[A](list: List[A]): List[A] =
    @tailrec
    def reverseIter[A](list: List[A], result: List[A]): List[A] =
      list match {
        case (h :: t) => reverseIter(t, h::result)
        case Nil => result
      }
    reverseIter(list, Nil)


  def goFromToCharTail(data: String, key: String): Boolean =
    @tailrec
    def goFromToCharIter(dataMarker: Int, keyMarker: Int, data: String, key: String, acc: Int): Boolean=
      if acc == key.length then true
      else if dataMarker < data.length && keyMarker < key.length then{
        if data.charAt(dataMarker) == key.charAt(keyMarker) then goFromToCharIter(dataMarker+1, keyMarker+1, data, key, acc+1)
        else if keyMarker == 0 then goFromToCharIter(dataMarker+1, 0, data, key, 0)
        else goFromToCharIter(dataMarker, 0, data, key, 0)
      }
      else false
    goFromToCharIter(0, 0, data, key, 0)


  def contains(data: String, key: String): Boolean=
    if key == "" then false
    else if data.length < key.length then false
    else goFromToCharTail(data, key)

  def containsKeys (data: String, keys: List[String]): Boolean =
    keys match {
      case h :: t => if contains(data, h) then true else containsKeys(data, t)
      case Nil => false
    }


  def find(list: List[String], key: String): List[String] =
    list match {
      case Nil => Nil
      case h :: t => if contains(h, key) == true then h :: find(t, key)
      else find(t, key)
    }

    //zlozonosc obliczeniowa n^2 (przechodzimy slowo litera po literze i liste po kazdym slowie)
    //zlozonosc pamieciowa

  def findTail(list: List[String], key: String): List[String] =
    @tailrec
    def findIter(list: List[String], result: List[String]): List[String] =
      list match {
        case Nil => result
        case h :: t => if contains(h, key) == true then findIter(t, h :: result)
        else findIter(t, result)
      }
    reverse(findIter(list, Nil))

  //zloznosc obliczeniowa n^2
  //zlozonosc pamieciowa

  def findKeys(list: List[String], keys: List[String]): List[String] =
    list match {
      case Nil => Nil
      case h :: t => if containsKeys(h, keys) == true then h :: findKeys(t, keys)
        else findKeys(t, keys)
    }

  //zlozonosc obliczeniowa n^4 (przechodzimy dwie listy stringow, kazde slowo litera po literze)
  //zlozonosc pamieciowa

  def findKeysTail(list: List[String], keys: List[String]): List[String] =
    @tailrec
    def findKeysIter(list: List[String], result: List[String]): List[String] =
      list match {
        case h :: t => if containsKeys(h, keys) == true then findKeysIter(t, h::result)
          else findKeysIter(t, result)
        case Nil => result
      }
    reverse(findKeysIter(list, Nil))

  //zlozonosc obliczeniowa n^4
  //zlozonosc pamieciowa stala


  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (h :: t, _, _) => h :: joinLists(t, list2, list3)
      case (Nil, h :: t, _) => h :: joinLists(list1, t, list3)
      case (Nil, Nil, h :: t) => h :: joinLists(list1, list2, t)
      case (Nil, Nil, Nil) => Nil

  //zlozonosc obliczeniowa n (suma dlugosci trzech list)
  //zlozonosc pamieciowa (3*(n^2 + n)/2) + 3n



  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    @tailrec
    def joinListsIter[A](list1: List[A], list2: List[A], result: List[A]): List[A] =
      (list1, list2) match
        case (h :: t, _) => joinListsIter(t, list2, h::result)
        case (Nil, h :: t) => joinListsIter(list1, t, h::result)
        case (Nil, Nil) => result
    joinListsIter(reverse(list2), reverse(list1), list3)

  //zlozonosc obliczeniowa n
  //zlozonosc pamieciowa stala

  def main(args: Array[String]): Unit ={
    //println(contains("alb", "ala"))
    //println(contains("iindex0168202","index0168"))

    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168"))
    println(findTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168"))

    println(findKeys(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")))
    println(findKeysTail(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168")))

    println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)))
    println(joinLists(List(5, 4, 3, 2), List(), List(9)))

    //println(reverse(List(1,2,3)))
    //println(reverse(List(1)))

    println(joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)))
    println(joinListsTail(List(5, 4, 3, 2), List(), List(9)))
  }

}
