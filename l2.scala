//l2 zadanie 1.
val list1 = 3::4::1::Nil
val list2 = Nil
val list3 = -3::(-4)::1::Nil
def sumList (xs:List[Int]) = {
  def recSumList (xs:List[Int], listSum:Int):Int =
    if xs == Nil then listSum
    else recSumList (xs.tail, listSum + xs.head)
  recSumList(xs, 0)
}
val s1 = sumList(list1)
val s2 = sumList(list2)
val s3 = sumList(list3)

//l2 zadanie 2.
val words = "How"::"are"::"you"::"today"::Nil;;
val words1 = "Have"::"a"::"nice"::"day"::Nil;;
val words2 = "Oh"::"my"::Nil;;
val words3 = Nil;;
val space = " ";;
def createSentence (wordList:List[String], endMark:String) = {
  def recCreateSentence (wordList:List[String], endMark:String, sentence:String):String =
    if wordList.length < 1 then sentence + endMark
    else recCreateSentence(wordList.tail, endMark, sentence + space + wordList.head)
  recCreateSentence (wordList, endMark, "")
}
val sen1 = createSentence(words, "?")
val sen2 = createSentence(words1, "!")
val sen3 = createSentence(words2, "...")
val sen4 = createSentence(words3, "")

//l2 zadanie  3. - zakladam ze pusta lista tez ma zwracac true

val numbers1 = 3::4::8::Nil;;
val numbers2 = -3::5::1::Nil;;
val numbers3 = Nil;;
val numbers4 = 20::4::0::Nil;;
def isPositive (numbers:List[Int]):Boolean =
  if numbers.length <1 then true
  else if numbers.head >0 then isPositive (numbers.tail)
  else false
val pos1 = isPositive (numbers1)
val pos2 = isPositive (numbers2)
val pos3 = isPositive (numbers3)
val pos4 = isPositive (numbers4)

//l2 zadanie 4.
val x1 = 3
val x2 = 0
val x3 = -2
def strongF (x:Int):Int = {
  if x < 0 then throw new Exception ("The number is smaller than zero")
  else if x == 0 then x + 1
  else x * strongF (x - 1)
}
val st1 = strongF (x1)
val st2 = strongF (x2)
val st3 = strongF (x3)
