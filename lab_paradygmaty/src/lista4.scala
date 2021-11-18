object lista4 {

  import scala.annotation.tailrec

  def reverse[A](listIn: List[A]): List[A] =
    @tailrec
    def revTail[A](listIn: List[A], listOut: List[A]): (List[A]) =
      if listIn == Nil then listOut
      else revTail(listIn.tail, listIn.head :: listOut)

    revTail(listIn, Nil)

  //Złożoność obliczeniowa: O(n), gdzie n - długość listy
  //Złożoność pamięciowa: O(1)

  //Zadanie 1

  def checkChars(element: String, template: String): Boolean =
    @tailrec
    def recHelper(element: String, restOfElementChars: String, restOfTemplateChars: String): Boolean =
      (restOfElementChars, restOfTemplateChars) match
        case ("", _) => false
        case (_, "") => true
        case (_, _) =>
          if restOfElementChars.head == restOfTemplateChars.head then recHelper(element, restOfElementChars.tail, restOfTemplateChars.tail)
          else recHelper(element.tail, element.tail, template)

    recHelper(element, element, template)

  //Złożoność obliczeniowa: O(n), gdzie n długość słowa
  //Złożoność pamięciowa: O(1)

  def checkPhrases(elemnet: String, templateList: List[String]): Boolean =
    templateList match
      case Nil => false
      case headOfTemplateList :: tailOfTemplateList =>
        if checkChars(elemnet, headOfTemplateList) then true
        else checkPhrases(elemnet, tailOfTemplateList)


  //Złożoność obliczeniowa: O(n), gdzie n ilość wzorców
  //Złożoność pamięciowa: O(n), gdzie n ilość wzorców

  def find(listOfElements: List[String], templateList: List[String]): List[String] =
    (listOfElements, templateList) match
      case (Nil, _) => Nil
      case (_, Nil) => listOfElements
      case (headElem :: tailElem, _) =>
        if checkPhrases(headElem, templateList) then headElem :: find(tailElem, templateList)
        else find(tailElem, templateList)

  //Złożoność obliczeniowa: O(n*m*t) - O(n^3), gdzie n to ilosć wyrazów, m to ilość wzorców, t to długość wyrazów
  //Złożoność pamięciowa: O(n^2) - suma ciągu arytmetycznego: (1+n)*n/2, gdzie n jak wyżej


  def findTail(listOfElements: List[String], templateList: List[String]): List[String] =
    @tailrec
    def findTailRec(listOfElements: List[String], templateList: List[String], result: List[String]): List[String] =
      (listOfElements, templateList) match
        case (Nil, _) => result
        case (_, Nil) => reverse(listOfElements)
        case (headElem :: tailElem, _) =>
          if checkPhrases(headElem, templateList) then findTailRec(tailElem, templateList, headElem :: result)
          else findTailRec(tailElem, templateList, result)

    reverse(findTailRec(listOfElements, templateList, Nil))

  //Złożoność obliczeniowa: O(n*m*t)~O(n^3), gdzie n,m i t tak wyżej
  //Złożoność pamięciowa: O(1)

  //Zadanie 2

  def joinLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1, list2) match
      case (h1 :: t1, _) => h1 :: joinLists(t1, list2, list3)
      case (Nil, h2 :: t2) => h2 :: joinLists(list1, t2, list3)
      case (Nil, Nil) => list3


  //Złożoność obliczeniowa: O(n), gdzie n to suma długości listy pierwszej i drugiej
  //Złożoność pamięciowa: O(n^2) - suma ciągu arytmetycznego: (1+n)*n/2 + m, gdzie n takie jak wyżej a m długość listy trzeciej

  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    @tailrec
    def joinListsRec[A](list1: List[A], list2: List[A], result: List[A]): List[A] =
      (list1, list2) match
        case (_, h2 :: t2) => joinListsRec(list1, t2, h2 :: result)
        case (h1 :: t1, Nil) => joinListsRec(t1, list2, h1 :: result)
        case (Nil, Nil) => result

    joinListsRec(reverse(list1), reverse(list2), list3)

  //Złożoność obliczeniowa: O(2n+2m+p), gdzie n to długośc pierwszej listy, m to długość drugiej, p to długość trzeciej
  //Złożoność pamięciowa: O(n+m+p), pamięć wynika tylko z długości listy końcowej bo dzięki rekursji ogonowej nie jest zajmowane miejsce w pamięci


  def main(args: Array[String]): Unit = {

    println(joinLists(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinLists(List(), List(4, 8), List(12)) == List(4, 8, 12))
    println(joinLists(List(1, 2, 3), List(), List(10)) == List(1, 2, 3, 10))
    println(joinLists(List(1, 9, 2, 2), List(1, 0, 0), List()) == List(1, 9, 2, 2, 1, 0, 0))
    println(joinLists(List(), List(), List()) == List())
    println()


    println(joinListsTail(List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9))
    println(joinListsTail(List(), List(4, 8), List(12)) == List(4, 8, 12))
    println(joinListsTail(List(1, 2, 3), List(), List(10)) == List(1, 2, 3, 10))
    println(joinListsTail(List(1, 9, 2, 2), List(1, 0, 0), List()) == List(1, 9, 2, 2, 1, 0, 0))
    println(joinListsTail(List(), List(), List()) == List())
    println()

    println(find(List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222",
      "index0169224"), List("index0168")) == List("iindex016802", "iindex0168211", "iindex0168210"))
    println(find(List("abaca", "babdea", "cacde"), List()) == List("abaca", "babdea", "cacde"))
    println(find(List("adba", "abaa", "cac","adac"), List("ada")) == List("adac"))
    println(find(List("abaa", "baba", "cac","dacb","caac"), List("aa", "ba","ca")) == List("abaa", "baba","cac","caac"))
    println()

    println(findTail(List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222",
      "index0169224"), List("index0168")) == List("iindex016802", "iindex0168211", "iindex0168210"))
    println(findTail(List("abaca", "babdea", "cacde"), List()) == List("abaca", "babdea", "cacde"))
    println(findTail(List("adba", "abaa", "cac","adac"), List("ada")) == List("adac"))
    println(findTail(List("abaa", "baba", "cac","dacb","caac"), List("aa", "ba","ca")) == List("abaa", "baba","cac","caac"))
    println()
  }
}
