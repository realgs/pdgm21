import scala.annotation.tailrec

object Lab4 {
  // zadanie 2
  def joinLists[A](first:List[A],second:List[A],third:List[A]):List[A] =
    (first,second,third) match
      case (Nil,Nil,Nil) => Nil
      case (Nil,_,_) => joinLists(second,third,Nil)
      case (_,Nil,Nil) => first
      case (h1::t1,_,_) => h1::joinLists(t1,second,third)
      //złożoność obliczeniowa dł.first + dł.second
      //złożoność pamięciowa 1...n (n-suma dł 3 list) = 1/2(n+n^2)

  def joinListsTail[A](first:List[A],second:List[A],third:List[A]):List[A] =
    def join[A](st:List[A],nd:List[A],rd:List[A],result:List[A]):List[A] =
      (st,nd,rd,result) match
        case (Nil,Nil,Nil,_) => result
        case (Nil,_,_,_) => join(nd,rd,Nil,result)
        case (h1::t1,_,_,_) => join(t1,nd,rd,h1::result)
    reverse(join(first,second,third,Nil))
    //złożoność obliczeniowa dł.first+dł.second+dł.third
    //złożoność pamięciowa n - długość wszystkich list

  def reverse[A](list:List[A]):List[A] =
    def rev[A](list:List[A], reversed:List[A]):List[A]=
      if list==Nil then reversed
      else rev(list.tail, list.head::reversed)
    rev(list,Nil)
    //złożonośc obliczeniowa n
    //złożoność pamięciowa n

  //zadanie 1
  def find(list:List[String],phrase:String):List[String] =
    (list,phrase) match
      case (Nil,_) => Nil
      case (_,"") => list
      case (h1::t1,_) => if contains(h1,phrase) then h1::find(t1,phrase) else find(t1,phrase)
    // złożoność obliczeniowa długość list * contains (średnia długość słów)
    // złożoność pamięciowa n

  def findTail(list1:List[String],phrase1:String):List[String] =
    def innerFindTail(list:List[String],phrase:String,result:List[String]):List[String] =
      (list,phrase) match
        case (Nil,_) => result
        case (_,"") => list
        case (h1::t1,_) => if contains(h1,phrase) then innerFindTail(t1,phrase,h1::result) else innerFindTail(t1,phrase,result)
    innerFindTail(list1,phrase1,List())
    // złożoność obliczeniowa contains * n
    // złożoność pamięciowa 1


  def find2(list:List[String],phrases:List[String]):List[String] =
    def innerLoop(words: List[String], phrasesLeft: List[String]): List[String] =
      (words, phrasesLeft) match
        case (Nil, _) => Nil
        case (h1::t1, Nil) => innerLoop(t1,phrases)
        case (h1::t1, h2::t2) => if contains(h1,h2) then h1::innerLoop(t1,phrases) else innerLoop(words,t2)
    innerLoop(list,phrases)
    //złożoność obliczeniowa contains * długość list * długość phrases
    //złożoność pamięciowa n


  def find2Tail(list:List[String],phrases:List[String]):List[String] =
    def innerLoopTail(words: List[String], phrasesLeft: List[String], result:List[String]): List[String] =
      (words, phrasesLeft) match
        case (Nil, _) => result
        case (h1::t1, Nil) => innerLoopTail(t1,phrases,result)
        case (h1::t1, h2::t2) => if contains(h1,h2) then innerLoopTail(t1,phrases,h1::result) else innerLoopTail(words,t2,result)
    innerLoopTail(list,phrases,List())
    //złożoność obliczeniowa contains * długość list * długość phrases
    //złożoność pamięciowa

  def contains(first:String,second:String):Boolean =
    @tailrec
    def help(word:String,tmpWord:String,phrase:String,tmpPhrase:String):Boolean =
      (tmpWord,tmpPhrase) match
        case (_,"") => true
        case ("",_) => false
        case _ => if tmpWord.head==tmpPhrase.head then help(word,tmpWord.tail,phrase,tmpPhrase.tail) else help(word.tail,word.tail,phrase,phrase)
    help(first,first,second,second)
    //złożoność obliczeniowa długość word + przesunięcia w tył(max n min 0)
    //złożoność pamięciowa chyba stała = 1


  def main(args: Array[String]) : Unit = {
    println(joinLists(List(1,2,3),List(4,5,6),List(7,8)))
    println(joinLists(List(),List(4,5,6),List(7,8)))
    println(joinLists(List(1,2,3),List(),List(7,8)))
    println(joinLists(List(1,2,3),List(4,5,6),List()))
    println(joinLists(List(),List(),List(7,8)))
    println(joinLists(List(),List(),List()))
    println("----------------------")
    println(joinListsTail(List(1,2,3),List(4,5,6),List(7,8)))
    println(joinListsTail(List(),List(4,5,6),List(7,8)))
    println(joinListsTail(List(1,2,3),List(),List(7,8)))
    println(joinListsTail(List(1,2,3),List(4,5,6),List()))
    println(joinListsTail(List(),List(),List(7,8)))
    println(joinListsTail(List(),List(),List()))
    println("----------------------")
    println(find(List("index0169" , "iindex0168202" , "iindex0168211" , "iindex0168210" , "iindex0169222" , "index0169224") , "index0168"))
    println(find(List("abba","abaa") , "aba"))
    println(findTail(List("index0169" , "iindex0168202" , "iindex0168211" , "iindex0168210" , "iindex0169222" , "index0169224") , "index0168"))
    println(find2(List("index0169" , "iindex0168202" , "iindex0168211" , "iindex0168210" , "iindex0169222" , "index0169224") , List("index0168","69")))
    println(find2Tail(List("index0169" , "iindex0168202" , "iindex0168211" , "iindex0168210" , "iindex0169222" , "index0169224") , List("index0168","69")))
}
}
