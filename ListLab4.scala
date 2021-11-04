object ListLab4 {
  def main(args: Array[String]): Unit = {
    println(combineLists(List("a","b","c"),List("d","e"),List("f")))
    println(combineLists(List(1,2,3),List(4,5),List(6)))
    println(combine(List(1,2,3),List(4,5),List(6)))

    println(" ")
    println(find (List(4442323,2423,36546,4234,5645,6234), 423))
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), "index0168"))
    println(find(List(5),5))
    println(find(List("dsfsdfsdf","gdfgdsgsda"),""))
    println(find(List(5,54,7),Nil))
    println(find(Nil,4))
    println(find(Nil,Nil))

    println(" ")
    println(findForList(List(4442323,2423,36546,4234,5645,624), List(23,46)))
    println(findForList(List("4442323","2423","36546","4234","5645","624"), List("23","46")))
    println(findForList(List("4442323","2423","36546","4234","5645","624"), List("23","46")))
    println(findForList(List(4442323,2423,36546,4234,5645,624), List()))
    println(findForList(List(4442323,2423,36546,4234,5645,624), List("","","23","")))
    println(findForList(List(), List("","","23","")))

    println(" ")
    println(findTail(List(4442323,2423,36546,4234,5645,6234), 423) )
    println(findTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), "index0168"))
    println(findTail(List(5),5))

    println(" ")
    println(findForListTail(List(4442323,2423,36546,4234,5645,6234), List(423,5643,24)) )
    println(findForListTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List("0169","222","224")))
    println(findForListTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List(0169,222,224)))
    println(findForListTail(List(5),List(5)))
    println(findForListTail(List(5,6,853245,2,34563),List("")))
    println(findForListTail(List(),List(5,6)))


  }

  def combineLists[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
    (list1,list2,list3) match
      case (List(), List(), List()) => List()
      case (h::t, _, _) => h :: combineLists(t,list2,list3)
      case (List(), h::t, _) => h :: combineLists(List(),t,list3)
      case (List(), List(), h::t) => h :: combineLists(List(),List(),t)

  def combine[A](list1: List[A], list2: List[A], list3: List[A]): List[A] ={
    def combineLists_2[A](list1: List[A], list2: List[A], list3: List[A], list: List[A]): List[A] =
      (list1,list2,list3) match
        case (List(), List(), List()) => list
        case (h::t, _, _) => combineLists_2(t,list2,list3,h::list)
        case (List(), h::t, _) => combineLists_2(List(),t,list3,h::list)
        case (List(), List(), h::t) => combineLists_2(List(),List(),t,h::list)
    combineLists_2(list1,list2,list3,List()).reverse}






  def include(list: List[Char], includingListMain: List[Char]): Boolean ={
    def includehelp(list: List[Char], includingList: List[Char]): Boolean = {
      (list,includingList) match
        case (_,List()) => true
        case (List(),_) => false
        case (h::t,h2::t2) => if h==h2 then includehelp(t,t2) else false
    }
    if(includingListMain.isEmpty){false}
    else {
      list match
        case List() => false
        case h :: t => if (h == includingListMain.head) {
          if includehelp(list, includingListMain) == true then true else include(t, includingListMain)
        } else include(t, includingListMain)
    }
  }


  def find[A](list: List[A], elementToFind: A): List[A] =
    list match
      case List() => List()
      case h::t => if include(h.toString.toList,elementToFind.toString.toList) then h::find(t,elementToFind) else find(t,elementToFind)


  def findForList[A](list: List[A], elementToFind: List[A]): List[A] ={
    def condition(suspect: A, elements: List[A]): Boolean =
      elements match
        case List() => false
        case h::t => if include(suspect.toString.toList,h.toString.toList) == true then true else condition(suspect,t)
    list match
      case List() => List()
      case h::t => if condition(h,elementToFind) then h::findForList(t,elementToFind) else findForList(t,elementToFind)}



  def findTail[A](list: List[A], elementToFind: A): List[A] ={
    def findRecTail[A](list: List[A], elementToFind: A, result: List[A]): List[A] =
      list match
        case List() => result
        case h::t => if include(h.toString.toList,elementToFind.toString.toList) then findRecTail(t,elementToFind,h::result) else findRecTail(t,elementToFind,result)
    findRecTail(list,elementToFind,List())
  }


  def findForListTail[A](list: List[A], elementsToFind: List[A]): List[A] = {
    def findForListRecTail[A](list: List[A], elementsToFind: List[A], result: List[A]): List[A] = {
      def condition(suspect: A, elements: List[A]): Boolean =
        elements match
          case List() => false
          case h :: t => if include(suspect.toString.toList, h.toString.toList) == true then true else condition(suspect, t)

      list match
        case List() => result
        case h :: t => if condition(h, elementsToFind) then findForListRecTail(t, elementsToFind,h::result) else findForListRecTail(t, elementsToFind, result)
    }
    findForListRecTail(list,elementsToFind,List())
  }



}
