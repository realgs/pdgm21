object Lab4 {

  def rev[A] (list: List[A]) : List[A] =
    def revHelper (list: List[A], result: List[A]) : List[A] =
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
    def joinLists2Helper[A](list1: List[A], list2: List[A], list3: List[A], result: List[A]): List[A] =
    {
      (list1, list2, list3) match
        case (Nil, Nil, Nil) => result
        case (h1 :: t1, list2, list3) => joinLists2Helper(t1, list2, list3, h1 :: result)
        case (Nil, h2 :: t2, list3) => joinLists2Helper(list1, t2, list3, h2 :: result)
        case (Nil, Nil, h3 :: t3) => joinLists2Helper(list1, list2, t3, h3 :: result)
    }
        rev(joinLists2Helper(list1, list2, list3, Nil))
     //złożoność obliczeniowa O(n) - dlatego, że używamy rev, którego złożoność jest liniowa
     //złożoność pamięciowa O(n^2) dokładanie (n^2+n)/2 + 3n , h1 :: result - złozoność liniowa

  def contains (sprawdzany : String, fraza : String) : Boolean =
    def containsHelper(sprawdzany : String,poprawny : String, fraza : String, oryginal : String) : Boolean =
    {
      if (fraza.isEmpty) then true
      else if(sprawdzany.length < fraza.length) then false
      else if sprawdzany.head == fraza.head then containsHelper(sprawdzany.tail, sprawdzany.tail, fraza.tail, oryginal)
      else if (fraza!= oryginal) then containsHelper(poprawny,poprawny, oryginal, oryginal)
      else  containsHelper(sprawdzany.tail,poprawny, oryginal, oryginal)
    }
       containsHelper(sprawdzany,null, fraza, fraza)




   def find (list: List[String], listaFraz : List[String]) : List[String] =
     def findHelper(list: List[String], listaFraz: List[String], oryginal: List[String]) : List[String] =
     {
       (list, listaFraz) match
         case (Nil, Nil) => Nil
         case (Nil, _) => Nil
         case (_, Nil) => Nil
         case (h1 :: t1, h2 :: t2) => if (contains(h1,h2)) then h1 :: findHelper(t1, oryginal, oryginal)
                                      else if (t2 != Nil) findHelper(list, t2, oryginal)
                                      else findHelper(t1, oryginal, oryginal)
     }
       findHelper(list, listaFraz, listaFraz)
       //złożoność obliczeniowa O(n) - dokładanie n
       //złożoność pamięciowa O(n^2) - (n^2+n)/2 +3n

  def find2 (list: List[String], listaFraz : List[String]) : List[String] =
    def find2Helper(list: List[String], listaFraz: List[String] ,oryginal: List[String], result: List[String]): List[String] =
    {
      (list, listaFraz) match
        case (Nil, Nil) => result
        case (Nil, _) => result
        case (_, Nil) => result
        case (h1 :: t1, h2 :: t2) => if (contains(h1, h2)) then find2Helper(t1, oryginal, oryginal, h1 :: result)
                                     else if (t2 != Nil) find2Helper(list, t2, oryginal, result)
                                     else find2Helper(t1, oryginal, oryginal, result)
    }
      find2Helper(list, listaFraz, listaFraz, Nil)
//tutaj tak samo
  def main(args: Array[String]): Unit = {

    println(joinLists(List(1, 2, 3), List(4, 5), List(6)))
    println(joinLists(List(), List(1,2,3), List(4,5)))
    println(joinLists(List(1,2,3), List(), List(6)))
    println(joinLists(List(1,2,3), List(4,5), List()))
    println(joinLists(List(1,2,3), List(), List()))
    println(joinLists2(List(1,2,3), List(4,5 ), List()))
    println(joinLists2(List(), List(1,2,3), List(4,5)))
    println(joinLists2(List(1,2,3), List(), List(6)))
    println(joinLists2(List(1,2,3), List(4,5), List()))
    println(joinLists2(List(1,2,3), List(), List()))
    println(find(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index0168", "index0169")))
    println(find2(List("index0169" ,"iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222"), List("index0168")))
    println(find(List(), List("aaaaa")))

  }


}
