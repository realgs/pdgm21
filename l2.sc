def sum(xs: List[Int]): Int =
  if xs == List() then 0 else xs.head + sum(xs.tail)

sum(List(1, 2, 3, 4)) == 10
sum(List()) == 0
sum(List(1, -1, 2)) == 2

def makeSentence(xs: List[String], x:Char): String =
  if xs == List() then x.toString
  else if xs.tail == List() then xs.head + x.toString
  else xs.head + " " + makeSentence(xs.tail, x)

makeSentence(List("Ala", "ma", "kota"), '.') == "Ala ma kota."
makeSentence(List("ya", "like", "jazz"), '?') == "ya like jazz?"
makeSentence(List(), '!') == "!"
