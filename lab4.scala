import annotation.tailrec

object Main {
  def main(args: Array[String]): Unit = {
    println(find3(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"index0168","22"))
    println(find4(List("index0169","iindex0168202","iindex0168211","iindex0168210","iindex0169222","index0169224"),"index0168","22"))

  }
}


//O(n) - everyletter check
def startsWithPhrase(word: String, phrase: String) = {
  if(phrase == "") then false
  def startsWithHelper(wordInner: String, phraseInner: String): Boolean =
    //println(wordInner + " " + phraseInner)
    (wordInner, phraseInner) match
      case (_, "") => true
      case ("", _) => false
      case (word, phrase) =>  if word.head == phrase.head then startsWithHelper(word.tail, phrase.tail)
                              else false
  startsWithHelper(word,phrase)
}
//O(n^2) - words letters
def containsPhrase(word: String, phrase: String) = {
  if(phrase == "") then false
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


def find3(list: List[String],values : String*):List[String] =
  list match
    case Nil => Nil
    case (h::t) => if(findInMultipleValues(h,values*)) then h :: find3(t,values*) else find3(t,values*)

//O(n^4) - every item in list
def find4(list: List[String],values : String*) =
  @tailrec
  def find4Helper(innerList: List[String], result: List[String],values : String* ):List[String] =
    innerList match
      case Nil => result
      case h::t=> if(findInMultipleValues(h,values*)) then find4Helper(t,h::result,values*) else find4Helper(t,result,values*)

  find4Helper(list,Nil,values*)




