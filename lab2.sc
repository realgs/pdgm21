

def sum(list :List[Int]):Int =
  if list == Nil then 0
  else list.head + sum(list.tail)

sum(List(1,2,3,4))


def makeSentence(list :List[String]):String =
  if list == Nil then throw new Exception("List need contains '.'")
  else if list.head == "." then throw new Exception("Sentence need to have 'body'")
  else if list.tail.head == "." then list.head + list.tail.head
  else list.head + " " + makeSentence(list.tail)

makeSentence(List("Ala","ma","kota","."))
makeSentence(List("Ala","ma","kota"))
makeSentence(List(".","Ala"))
makeSentence(List("Ala","ma",".","kota","."))