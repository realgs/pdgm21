object l4 {
  def main(args: Array[String]): Unit = {
    println(joinLists(List(5,4,3,2), List(1,0), List(9)))
    println(joinLists(List('X', 'Y', 'Z'), List('A'), List()))
    println(joinLists(List(), List(), List()))

    println(joinLists2(List(5,4,3,2), List(1,0), List(9)))
    println(joinLists2(List('X', 'Y', 'Z'), List('A'), List()))
    println(joinLists2(List(), List(), List()))


    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168", "index169")))
    println(find(List("Magdalena", "Marlena", "Milena", "Monika", "Matylda", "Mira"), List("Mi", "Mo")))
    println(find(List("dom", "złom", "łom", "om", "młyn"), List("om")))
    println(find2(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168")))
    println(find2(List("Magdalena", "Marlena", "Milena", "Monika", "Matylda", "Mira"), List("Mi", "Mo")))
    println(find2(List("dom", "złom", "łom", "om", "młyn"), List("om")))




  }
  
  //zadanie 1
  def find(list:List[String], phraseList:List[String]) = {
    def helper(list: List[String], phraseList: List[String], original: List[String]): List[String] =
      (list, phraseList) match {
        case (Nil, Nil) => Nil
        case (Nil, _) => Nil
        case (_, Nil) => Nil
        case (h1 :: t1, h2 :: t2) =>
          if (contains(h1, h2)) then h1 :: helper(t1, original, original)
          else if (t2 != Nil) then helper(list, t2, original)
          else helper(t1, original, original)
      }
    helper(list, phraseList, phraseList)
  }

  def find2(list:List[String], phraseList:List[String]) = {
    def helper2(list: List[String], phraseList: List[String], original: List[String], result:List[String]): List[String] =
      (list, phraseList) match {
        case (Nil, Nil) => result
        case (Nil, _) => result
        case (_, Nil) => result
        case (h1 :: t1, h2 :: t2) =>
          if (contains(h1, h2)) then helper2(t1, original, original, h1::result)
          else if (t2 != Nil) helper2(list, t2, original, result)
          else helper2(t1, original, original, result)
      }
    helper2(list, phraseList, phraseList, Nil).reverse
  }



  def contains(check:String, phrase:String):Boolean =
    if (check == "" && phrase =="") then true
    else if (phrase == "") then true
    else if (check == "") then false
    else if (check.head == phrase.head) then contains(check.tail, phrase.tail)
    else contains(check.tail, phrase)

  //zadanie 2
  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2, list3) match
      case (Nil, Nil, Nil) => Nil
      case (Nil, h2 :: t2, _) => h2 :: joinLists(list1, t2, list3)
      case (Nil, Nil, h3 :: t3) => h3 :: joinLists(list1, list2, t3)
      case (h1 :: t1, _, _) => h1 :: joinLists(t1, list2, list3)


  def joinLists2[A](list1: List[A], list2: List[A], list3: List[A]) =
    def joinListsHelper[A](list1: List[A], list2: List[A], list3: List[A], resultList: List[A]): List[A] =
      (list1, list2, list3) match
        case (Nil, Nil, Nil) => resultList
        case (h1 :: t1, _, _) => joinListsHelper(t1, list2, list3, h1 :: resultList)
        case (Nil, h2 :: t2, _) => joinListsHelper(list1, t2, list3, h2 :: resultList)
        case (Nil, Nil, h3 :: t3) => joinListsHelper(list1, list2, t3, h3 :: resultList)
    joinListsHelper(list1, list2, list3, Nil).reverse


  def reverse[A](list: List[A]) =
    def helper[A](list: List[A], result: List[A]): List[A] =
      list match
        case Nil => result
        case h :: t => helper(t, h :: result)
    helper(list, Nil)
}