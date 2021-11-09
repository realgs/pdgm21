object Lab4_2 {

  def rev[A](list: List[A]): List[A] =
    def revHelper(list: List[A], result: List[A]): List[A] =
      list match
        case Nil => result
        case h :: t => revHelper(t, h :: result)

    revHelper(list, Nil)

  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (Nil, Nil, Nil) => Nil
      case (h1 :: t1, list2, list3) => h1 :: joinLists(t1, list2, list3)
      case (Nil, h2 :: t2, list3) => h2 :: joinLists(list1, t2, list3)
      case (Nil, Nil, h3 :: t3) => h3 :: joinLists(list1, list2, t3)

  //zlożoność obliczeniowa O(n) - dokładanie n
  //złożoność pamięciowa 0(n^2) - dokładanie (n^2+n)/2 +3n

  def joinLists2[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    def joinLists2Helper[A](list1: List[A], list2: List[A], list3: List[A], result: List[A]): List[A] = {
      (list1, list2, list3) match
        case (Nil, Nil, Nil) => result
        case (h1 :: t1, list2, list3) => joinLists2Helper(t1, list2, list3, h1 :: result)
        case (Nil, h2 :: t2, list3) => joinLists2Helper(list1, t2, list3, h2 :: result)
        case (Nil, Nil, h3 :: t3) => joinLists2Helper(list1, list2, t3, h3 :: result)
    }

    rev(joinLists2Helper(list1, list2, list3, Nil))
  //złożoność obliczeniowa O(n) - dlatego, że używamy rev, którego złożoność jest liniowa
  //złożoność pamięciowa O(n^2) dokładanie (n^2+n)/2 + 3n , h1 :: result - złozoność liniowa

  def contains(sprawdzany: String, fraza: String): Boolean =
    def containsHelper(spr: String, fr: String): Boolean = {
      if (fr.isEmpty) then true
      else if (spr.length < fr.length) then false
      else if spr.head == fr.head then containsHelper(spr.tail, fr.tail)
      else if (fr != fraza) then containsHelper(spr, fraza)
      else containsHelper(spr.tail, fr)
    }

    containsHelper(sprawdzany, fraza)


  def find(list: List[String], listaFraz: List[String]): List[String] =
    def findHelper(list: List[String], lista: List[String]): List[String] = {
      (list, lista) match
        case (Nil, Nil) => Nil
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (h1 :: t1, h2 :: t2) => if (contains(h1, h2)) then h1 :: findHelper(t1, listaFraz)
                                     else if (t2 != Nil) findHelper(list, t2)
                                     else findHelper(t1, listaFraz)
    }

    findHelper(list, listaFraz)
  //złożoność obliczeniowa O(n) - dokładanie n
  //złożoność pamięciowa O(n^2) - (n^2+n)/2 +2n

  def find2(list: List[String], listaFraz: List[String]): List[String] =
    def find2Helper(list: List[String], lista: List[String], result: List[String]): List[String] = {
      (list, lista) match
        case (Nil, Nil) => result
        case (Nil, _) => result
        case (_, Nil) => result
        case (h1 :: t1, h2 :: t2) => if (contains(h1, h2)) then find2Helper(t1, listaFraz, h1 :: result)
                                     else if (t2 != Nil) find2Helper(list, t2, result)
                                     else find2Helper(t1, listaFraz, result)
    }

    find2Helper(list, listaFraz, Nil)
  //to samo
  def main(args: Array[String]): Unit = {

    println(joinLists(List(1, 2, 3), List(4, 5), List(6)))
    println(joinLists(List(), List(1, 2, 3), List(4, 5)))
    println(joinLists(List(1, 2, 3), List(), List(6)))
    println(joinLists(List(1, 2, 3), List(4, 5), List()))
    println(joinLists(List(1, 2, 3), List(), List()))
    println(joinLists2(List(1, 2, 3), List(4, 5), List()))
    println(joinLists2(List(), List(1, 2, 3), List(4, 5)))
    println(joinLists2(List(1, 2, 3), List(), List(6)))
    println(joinLists2(List(1, 2, 3), List(4, 5), List()))
    println(joinLists2(List(1, 2, 3), List(), List()))
    println(find(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index0168", "index0169")))
    println(find2(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index0168")))
    println(find(List(), List("aaaaa")))

  }

}
