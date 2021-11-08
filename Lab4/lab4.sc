import scala.annotation.tailrec



def containsFrase(inputString: String, frase: String): Boolean={
  if frase.isBlank then true
  else if inputString.isBlank then false
  else if inputString.head == frase.head then
    containsFrase(inputString.tail, frase.tail)
  else containsFrase(inputString.tail, frase)
}

def containsFrases(inputString: String, frases: List[String]): Boolean={
  if frases == Nil then false
  else containsFrase(inputString, frases.head) || containsFrases(inputString, frases.tail)
}

def find(list: List[String], frases: List[String]): List[String] ={
  list match
    case h::t => if containsFrases(h, frases) then h :: find(t, frases)
                          else find(t, frases)
    case Nil => Nil
}
//StringSeq doesn't work

def findTail(list: List[String], frases: List[String]): List[String]= {
  @tailrec
  def iterlist(list: List[String], frases: List[String], result: List[String]): List[String]={
    list match
      case h::t => if containsFrases(h, frases) then iterlist(t, frases, h :: result)
        else iterlist(t, frases, result)
      case Nil => result
  }
  iterlist(list, frases, List())
}

find(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List("index0168"))
findTail(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"), List("index0168"))






def joinLists[A](first: List[A], second: List[A], third: List[A]): List[A] = {
  (first, second) match
    case (h1::t1, l2) => h1 :: joinLists(t1, l2, third)
    case (Nil, h2::t2) => h2 :: joinLists(Nil, t2, third)
    case (Nil, Nil) => third
}

def reverseList[A](list: List[A]): List[A] ={
  @tailrec
  def innerReverse[A](list: List[A], reversedList: List[A]): List[A] ={
    list match
      case h1:: t1 => innerReverse(t1, h1 :: reversedList)
      case Nil => reversedList
  }
  innerReverse(list, List())
}

def joinListsTail[A](first: List[A], second: List[A], third: List[A]): List[A] = {
  @tailrec
  def iter[A](first: List[A], second: List[A], third: List[A], finalList: List[A]): List[A] ={
    (first, second, third) match
      case (h1::t1, l2, l3) => iter(t1, l2, l3, h1:: finalList)
      case (Nil, h2::t2, l3) => iter(Nil, t2, l3, h2 :: finalList)
      case (Nil, Nil, h3::t3) => iter(Nil, Nil, t3, h3 :: finalList)
      case(Nil, Nil, Nil) => finalList
  }
  reverseList(iter(first, second, third, List()))
}

val a = List(1,2,3,4,5,6)
val b = List(7,8,9)
val c = List(10,11,12)
joinLists(a,b,c)
joinListsTail(a,b,c)