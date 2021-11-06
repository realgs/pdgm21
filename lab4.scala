import annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    println(find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"index0168","22")
      == List("iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"))
    println(findTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"index0168","22")
      == List("iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"))

    println(find(Nil,"xxx") == Nil)
    println(findTail(Nil,"xxx") == Nil)

    println(find(List("index0169","iindex0168202","iindex0168211"),"") == Nil)
    println(findTail(List("index0169","iindex0168202","iindex0168211"),"") == Nil)

    println(joinLists(List(5,4,3,2),List(1,0),List(9)) == List(5,4,3,2,1,0,9))
    println(joinListsTail(List(5,4,3,2),List(1,0),List(9)) == List(5,4,3,2,1,0,9))

    println(joinLists(List(),List(),List(5,4,3,2)) == List(5,4,3,2))
    println(joinListsTail(List(),List(),List(5,4,3,2)) == List(5,4,3,2))

    println(joinLists(List("A","B"),List(),List("C")) == List("A","B","C"))
    println(joinListsTail(List("A","B"),List(),List("C")) == List("A","B","C"))
  }
}


//O(n) - everyletter check
def startsWithPhrase(word: String, phrase: String):Boolean = {
  if(phrase == "") then return false
  def startsWithHelper(wordInner: String, phraseInner: String): Boolean =
    (wordInner, phraseInner) match
      case (_, "") => true
      case ("", _) => false
      case (word, phrase) =>  if word.head == phrase.head then startsWithHelper(word.tail, phrase.tail)
                              else false
  startsWithHelper(word,phrase)
}
//O(n^2) - words letters
def containsPhrase(word: String, phrase: String):Boolean = {
  if(phrase == "") then return false
  def containsPhraseHelper(innerWord: String):Boolean =
    //println(innerWord + " " + phrase)
    if(innerWord == "") then false
    else if(startsWithPhrase(innerWord,phrase)) then true
     else containsPhraseHelper(innerWord.tail)

  containsPhraseHelper(word)
}


//O(n^3) - values
def findInMultipleValues(word: String, values : String*):Boolean =
  if values == Nil then false
  else if containsPhrase(word,values.head) then true
  else findInMultipleValues(word,values.tail*)

//O(n^4) - every item in list
def find(list: List[String],values : String*):List[String] =
  list match
    case Nil => Nil
    case (h::t) => if(findInMultipleValues(h,values*)) then h :: find(t,values*) else find(t,values*)

//O(n^4) - every item in list
def findTail(list: List[String],values : String*) =
  @tailrec
  def findHelper(innerList: List[String], result: List[String],values : String* ):List[String] =
    innerList match
      case Nil => result
      case h::t=> if(findInMultipleValues(h,values*)) then findHelper(t,h::result,values*) else findHelper(t,result,values*)

  reverseList(findHelper(list,Nil,values*))



//2
//O(n+m+k)
def joinLists[A](first :List[A], second :List[A], third :List[A]):List[A] =
  (first,second,third) match
    case (h::t,_,_) => h::joinLists(t,second,third)
    case (Nil,h::t,_) => h::joinLists(Nil,t,third)
    case (Nil,Nil,h::t) => h::joinLists(Nil,Nil,t)
    case (Nil,Nil,Nil) => Nil

//O(2*(n+m+k)
def joinListsTail[A](first :List[A], second :List[A], third :List[A]) =
  @tailrec
  def joinListsHelper[A](first :List[A], second :List[A], third :List[A], result :List[A]):List[A] =
    (first,second,third) match
      case (h::t,_,_) => joinListsHelper(t,second,third,h::result)
      case (Nil,h::t,_) => joinListsHelper(Nil,t,third,h::result)
      case (Nil,Nil,h::t) => joinListsHelper(Nil,Nil,t,h::result)
      case (Nil,Nil,Nil) => reverseList(result)
  joinListsHelper(first,second,third,Nil)

//O(n)
def reverseList[A](list :List[A]) = {
  @tailrec
  def reverseListHelper[A](srcList :List[A], destList :List[A]):List[A] =
    if srcList == Nil then destList
    else reverseListHelper(srcList.tail, srcList.head::destList)


  reverseListHelper(list,Nil)
}




