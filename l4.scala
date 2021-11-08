// Monika Jung
// list 4
// task 1

def listLeng[A] (listA:List[A]):Int ={
  def recListLeng[A] (listA:List[A], result:Int):Int={
    if listA == Nil then result
    else recListLeng(listA.tail, result+1)
  }
  recListLeng(listA,0)
}

def stringLeng (StringA:String):Int ={
  def recStringLeng (StringA:String, result:Int):Int={
    if StringA == "" then result
    else recStringLeng(StringA.tail, result+1)
  }
  recStringLeng(StringA,0)
}

def checkOneWord(sourceWord:String, patternWord:String):Boolean={
  def recCheckOneWord (sourceWordTail:String, patternWordTail:String, check:Boolean):Boolean = {
    if stringLeng(sourceWordTail) < stringLeng(patternWordTail) then false
    else if patternWordTail == "" && check == true then true
    else if patternWordTail == "" then false
    else if patternWordTail.head == sourceWordTail.head then recCheckOneWord(sourceWordTail.tail, patternWordTail.tail, true)
    else if patternWordTail.head != sourceWordTail.head then recCheckOneWord(sourceWordTail.tail, patternWord, false)
    else false
  }
  recCheckOneWord(sourceWord, patternWord, false)
}

checkOneWord ("aladadata", "dad")
checkOneWord ("alaata", "dad")
checkOneWord ("a", "dad")
checkOneWord ("alaata", "")

def checkOnePattern(sourceWordList:List[String], patternWord:String):(List[String],List[String])={
  def recCheckOnePattern(sourceWordList:List[String], patternWord:String, resultList:List[String], notFoundList:List[String]):(List[String],List[String])={
    if sourceWordList == Nil then (resultList,notFoundList)
    else if checkOneWord(sourceWordList.head,patternWord) == true
    then recCheckOnePattern(sourceWordList.tail, patternWord, resultList:::List(sourceWordList.head), notFoundList)
    else recCheckOnePattern(sourceWordList.tail, patternWord, resultList, notFoundList:::List(sourceWordList.head))
  }
  recCheckOnePattern(sourceWordList, patternWord, Nil, Nil)
}
checkOnePattern (List("good_dadata", "bad_dstdgr","good_dall11dad"), "dad")

def checkAllPatterns (sourceWordList:List[String], patternWordList:List[String]):List[String]={
  def recCheckAllPatterns (lists_Res_Oth:(List[String],List[String]), patternList:List[String], resultList:List[String]):List[String]={
    if patternList == Nil then resultList
    else if lists_Res_Oth._2 == Nil then resultList
    else {
      val temp_list_Res_Oth = checkOnePattern(lists_Res_Oth._2, patternList.head)
      recCheckAllPatterns(temp_list_Res_Oth, patternList.tail, resultList:::temp_list_Res_Oth._1)
    }
  }
  recCheckAllPatterns((Nil,sourceWordList),patternWordList,Nil)
}
checkAllPatterns (List("good_dadata", "bad_a23ldald24","good_ddfgll",""), List("dad","ll","aaam","", "dl"))
checkAllPatterns (List("index01688","iindex018202","yindex0168211","iiiiindex01682210","iindex0169222","index0169224"), List("index0168"))

//task 2

def mergeTwoLists[A] (listA:List[A], listB:List[A]):List[A]={
  def recMergeTwoLists[A] (listA:List[A], listB:List[A], result:List[A]):List[A]={
    if listA == Nil && listB == Nil then result
    else if listA == Nil then recMergeTwoLists (listA, listB.tail, result:::List(listB.head))
    else recMergeTwoLists (listA.tail, listB, result:::List(listA.head))
  }
  recMergeTwoLists (listA, listB, Nil)
}

def mergeLists[A] (lists:List[List[A]]):List[A]={
  def recMergeLists[A] (lists:List[List[A]], result:List[A]):List[A]={
    if lists == Nil then result
    else recMergeLists(lists.tail, mergeTwoLists(result, lists.head))
  }
  recMergeLists (lists, Nil)
}

val list1 = "1"::"2"::"3"::"4"::"5"::"6"::Nil
val list2 = "m"::"a"::"o"::"a"::"m"::Nil
val list3 = "ala"::" "::"ma"::" "::"kota"::" "::"i"::" "::"psa"::Nil

val lists = List(list1,list2,list3)

mergeTwoLists(list1,list2)
mergeLists(lists)