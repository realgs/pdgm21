object Lista4Lab {


  def substringList(list1: List[String], listOfElements: List[String]): List[String] = {

    def matchSubstring(partOfList: String, element: String, keyWord: String): String = {
      if (partOfList == "") {
        ""
      } else if (element == "") {
        keyWord
      } else {
        if (partOfList.head == element.head) {
          matchSubstring(partOfList.tail, element.tail, keyWord);
        } else
          matchSubstring(partOfList.tail, element, keyWord);
      }
    }

    def matchLists(list1: List[String], listOfElements: List[String]): List[String] = {
      (list1, listOfElements) match {
        case (list1, h :: t) => matchElements(list1, h) ::: matchLists(list1, t)
        case (list1, Nil) => Nil
      }
    }

    def matchElements(list1: List[String], element: String): List[String] = {
      (list1, element) match {
        case (Nil, element) => Nil
        case (h :: t, element) => matchSubstring(h, element, h) :: matchElements(t, element)
      }
    }

    matchLists(list1, listOfElements).filter(_ != "");


  }




  /*
def substringList2(list1: List[String], elemList: List[String]): List[String] = {
  def substringListHelper(list1: List[String], elem: String): List[String] = {
    (list1) match {
      case (Nil, elem) => Nil
      case (h :: t, elem) => if ()
    }
  }
  substringListHelper(list1, elem);
}


 */


  def addLists[T](list1: List[T], list2: List[T], list3: List[T]): List[T] = {
    (list1, list2, list3) match {
      case (h :: t, list2, list3) => h :: addLists(t, list2, list3)
      case (Nil, h2 :: t2, list3) => h2 :: addLists(Nil, t2, list3)
      case (Nil, Nil, h3 :: t3) => h3 :: addLists(Nil, Nil, t3)
      case (Nil, Nil, Nil) => Nil
    }

  }

  def addLists2[T](list1: List[T], list2: List[T], list3: List[T]): List[T] = {
    def addListsHelper[T](list1: List[T], list2: List[T], list3: List[T], acc: List[T]): List[T] = {
      (list1, list2, list3) match {
        case (h :: t, list2, list3) => addListsHelper(t, list2, list3, h :: acc)
        case (Nil, h2 :: t2, list3) => addListsHelper(Nil, t2, list3, h2 :: acc)
        case (Nil, Nil, h3 :: t3) => addListsHelper(Nil, Nil, t3, h3 :: acc)
        case (Nil, Nil, Nil) => acc.reverse
      }
    }

    addListsHelper(list1, list2, list3, Nil);
  }

  def main(args: Array[String]): Unit = {
    println(addLists(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)));
    println(substringList(List("ADA1Masd", "SADdsa3SD", "ADaSA", "hubaobfs"), List("ADA", "SAD")));

  }

}

