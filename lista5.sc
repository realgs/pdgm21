/// Dawid Krutul
import scala.annotation.tailrec

/// Zadanie 2

def joinLists[A](ListX: List[A], ListY: List[A], ListZ: List[A]): List[A] =
  (ListX,ListY,ListZ) match
    case(headX :: tailX, ListY, ListZ) => headX :: joinLists(tailX, ListY, ListZ)
    case(_, headY :: tailY, ListZ) => headY :: joinLists(Nil, tailY, ListZ)
    case(_, _, headZ :: tailZ) => headZ :: joinLists(Nil, Nil, tailZ)
    case(_, _, _) => Nil


joinLists(List(1,2,3),List(4,5,6),List(7,8,9))  == List(1,2,3,4,5,6,7,8,9)
joinLists(List(1,2,3),List(),List(7,8,9))       == List(1,2,3,7,8,9)
joinLists(List(),List(4,5,6),List(7,8,9))       == List(4,5,6,7,8,9)

def joinListsTail[A](ListX: List[A], ListY: List[A], ListZ: List[A]): List[A] =
  @tailrec
    def joinListsTailHelp[A](ListX: List[A], ListY: List[A], ListZ: List[A], res: List[A]): List[A] =
        (ListX,ListY,ListZ) match
            case(headX :: tailX, listY, listZ) => joinListsTailHelp(tailX, listY, listZ, headX :: res)
            case(Nil, headY :: tailY, listZ) => joinListsTailHelp(Nil, tailY, listZ, headY :: res)
            case(Nil, Nil, headZ :: tailZ) => joinListsTailHelp(Nil, Nil, tailZ, headZ :: res)
            case(Nil,Nil,Nil) => res
    joinListsTailHelp(ListX, ListY, ListZ, Nil)


def revList[A](list: List[A]): List[A] =
  def revListHelp(list: List[A], res: List[A]): List[A] =
      list match
        case Nil => res
        case listHead :: listTail => revListHelp(listTail, listHead :: res)
  revListHelp(list,Nil)



revList(joinListsTail(List(1,2,3),List(4,5,6),List(7,8,9))) == List(1,2,3,4,5,6,7,8,9)
revList(joinListsTail(List(1,2,3),List(),List(7,8,9)))      == List(1,2,3,7,8,9)
revList(joinListsTail(List(),List(4,5,6),List(7,8,9)))      == List(4,5,6,7,8,9)


/// Zadanie 1

def compareLetterByLetter[A](word: String, pattern: String): Boolean =
  (word,pattern) match
    case(_,"") => true
    case("",_) => false
    case(_,_) => if word.head == pattern.head then compareLetterByLetter(word.tail,pattern.tail) else false

def chooseWordAndCheckWithSinglePattern[A](word: String, pattern: String): Boolean =
  (word,pattern) match
    case (_,"") => true
    case ("",_) => false
    case (_,_) => compareLetterByLetter(word,pattern) || chooseWordAndCheckWithSinglePattern(word.tail,pattern)

def chooseWordAndCheckWithPatterns[A](word: String, patterns: List[String]): Boolean =
  if patterns == Nil then false
  else if chooseWordAndCheckWithSinglePattern(word,patterns.head) == true then true
  else  chooseWordAndCheckWithPatterns(word, patterns.tail)

def findMatch[A](words: List[String], patterns: List[String]): List[String] =
    if words == Nil then Nil
    else if patterns == Nil then words
    else if chooseWordAndCheckWithPatterns(words.head,patterns) then words.head :: findMatch(words.tail, patterns)
    else findMatch(words.tail, patterns)



findMatch(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),List("index0168")) == List("iindex0168202", "iindex0168211", "iindex0168210")
findMatch(List("kurde","jest","w","pyte"),List("e","es")) == List("kurde","jest","pyte")

