object L4 {

  //obliczeniowa o(a+b+c)
  // pamieciowa o((c^2+c)/2 +(b^2+b)/2 +(a^2+a)/2)
  //a,b,c - listy

  def joinLists[T](list1:List[T],list2:List[T],list3:List[T]):List[T] = {
    (list1,list2,list3)match {
      case (h::t,list2,list3) => h::joinLists(t,list2,list3)
      case (Nil,h::t,list3)=> h:: joinLists(Nil,t,list3)
      case (Nil,Nil,h::t)=> h::joinLists(Nil,Nil,t)
      case(Nil,Nil,Nil)=> Nil
    }
  }

  //obliczeniowa o(a+b+c +o(reverse))
  // pamieciowa o(1)
  //a,b,c - listy
  def joinListsTail[A](list1: List[A], list2: List[A], list3: List[A]): List[A] =
  {
    def helper[A](list1: List[A], list2: List[A], list3: List[A], res: List[A]): List[A] =
    {
      (list1, list2, list3) match {
        case (Nil, Nil, Nil) => reverse1(res)
        case (Nil, Nil, h3 :: t3) => helper(Nil, Nil, t3, h3 :: res)
        case (Nil, h2 :: t2, list3) => helper(Nil, t2, list3, h2 :: res)
        case (h1 :: t1, list2, list3) => helper(t1, list2, list3, h1 :: res)
      }
    }
    helper(list1, list2, list3, Nil)
  }


  //pamieciowa o(1)
  // obliczeniowa o(n)
  // n- list


  def reverse1[A](list: List[A]): List[A] =
  {
    def helper[A](list: List[A], res: List[A]): List[A] = {
      if list == Nil then res
      else helper(list.tail, list.head :: res)
    }
    helper(list, Nil)
  }

//pamieciowa o(1) (tailrec)
// obliczeniowa o((n-k)*k)=> nk-k^2
//gdzie n-word,k-key
  def contains1(word:String,key:String,wordIter:Int,keyIter:Int): Boolean ={
    if wordIter>word.length-key.length then false
    else if keyIter==key.length then true
    else if word.charAt(wordIter+keyIter)==key.charAt(keyIter) then contains1(word,key,wordIter,keyIter+1)
    else contains1(word,key,wordIter+1,0)
  }

//obliczeniowa o(n * o(contains))
//pamieciowa  (n^2+n)/2 - wordlista
//gdzie n-wordlist k-key
  def find1(wordList:List[String], key:String): List[String] ={ if key == "" then Nil
    else wordList match {
      case head::tail => if contains1(head,key,0,0) then head::find1(tail,key) else find1(tail,key)
      case Nil =>Nil
    }
  }


  //obliczeniowa o(n * o(contains)+o(reverse))
  //pamieciowa o(1)
  // n-wordlist
  def find1Tail(phraseList: List[String], pattern: String): List[String] =
  {
    def helper(wordList: List[String], key: String, res: List[String]): List[String] =
    {
      wordList match {
        case Nil => reverse1(res)
        case h :: t => if contains1(h, key,0,0) then helper(t, key, h :: res) else helper(t, key, res)
      }
    }
    if pattern == "" then Nil
    else helper(phraseList, pattern, Nil)
  }
//nFraz
// obliceniowa n* o(contains)
//pamieciowa o(1)
//n- keyList
  def contains2(word: String, keyList: List[String]): Boolean =
  {
    keyList match {
      case Nil => false
      case head :: tail => if head != "" && contains1(word, head,0,0) then true else contains2(word, tail)
    }
  }

  //obliczeniowa n*o(conatains2)
  //pamieciowa (n^2+n)/2
  // n - wordlist


  def find2(wordList: List[String], keyList: List[String]): List[String] =
  {
    (wordList, keyList) match {
      case (Nil, _) => Nil
      case (_, Nil) => Nil
      case (headWord :: tailWord, _) => if contains2(headWord, keyList) then headWord :: find2(tailWord, keyList)
      else find2(tailWord, keyList)
    }
  }

  //obliczeniowa o(o(reverse1)+n*o(contains2))
  //pamieciowa o(1)
  //n - wordList


  def find2Tail(wordList: List[String], keyList: List[String]): List[String] =
  {
    def helper(wordList: List[String], keyList: List[String], res: List[String]): List[String] =
    {
      (wordList, keyList) match {
        case (Nil, _) => reverse1(res)
        case (_, Nil) => reverse1(res)
        case (headWord :: tailWord, _) => if contains2(headWord, keyList) then helper(tailWord, keyList, headWord::res)
        else helper(tailWord, keyList, res)
      }
    }
    helper(wordList, keyList, Nil)
  }



  def main (args:Array[String]): Unit = {
    print(joinListsTail(List(5,4,3,2),List(1,0),List(9)))
    //print(contains1("aaabcaa","abc",0,0))
    print(find1Tail(List("ala","lala","lalal","AKSwqdwdwdwdala","alslaleewew"),"ala")==List("ala", "lala", "lalal", "AKSwqdwdwdwdala"))
    println(find2Tail(List("Stal", "Gorzów", "mistrzem", "Polski"), List("Gorzów", "mistrz")) == List("Gorzów", "mistrzem"))
  }
}
