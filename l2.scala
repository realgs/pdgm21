class l2 {

  //zad 1
  def sum (list: List[Int]): Int =
    if list == Nil then 0 else list.head + sum(list.tail)

  sum(List(1, 2, 3, 4, 5))
  sum(List(-2, -1, 0 ,5, -6, 3))
  sum(List())

  //zad 2
  def endOfSentence (string: String): Boolean =
    if string == "." || string == "?" || string == "!" then true else false

  def sentence (list: List[String]): String =
    if list == Nil then throw new Exception("Empty list!")
    if endOfSentence(list.head) then list.head
    else {
      list.head +
        (if list.tail == Nil then throw new Exception("The list has no end!")
        else if !endOfSentence(list.tail.head) then " " else "")
        + sentence(list.tail)
    }

  sentence(List("Ala", "ma", "kota", "."))
  sentence(List("1", "2", "3", "!"))
  sentence(List("Ala", "ma", "kota"))
  sentence(List())

  //zad 3
  def ifGreaterThen0 (list: List[Double]): Boolean =
    if list == Nil then true
    else if list.head > 0 && ifGreaterThen0(list.tail) then true else false

  ifGreaterThen0(List(1, 2, 3, 4))
  ifGreaterThen0(List(-1, 2, 3, 4))
  ifGreaterThen0(List())

  //zad 4
  def factorial (n: Int): Int =
    if n < 0 then throw new Exception("Negative number!")
    if n == 0 then 1 else n * factorial(n - 1)

  factorial(5)
  factorial(0)
  factorial(-2)
}
