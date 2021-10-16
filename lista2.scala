//zadanie 1

val listSum: List[Int] => Int = (xs: List[Int]) =>
  if xs == Nil then 0
  else xs.head + listSum (xs.tail)

listSum (Nil) == 0
listSum (List(2)) == 2
listSum (List(-2, 3, -4, 1)) == -2

//zadanie 2

val sentenceFromList: List[String] => String = (xs: List[String]) =>
  if xs == Nil then ""
  else if xs.tail == Nil then xs.head
  else xs.head + " " + sentenceFromList (xs.tail)

sentenceFromList (Nil) == ""
sentenceFromList (List("!")) == "!"
sentenceFromList (List("zadanie", "drugie", ".")) == "zadanie drugie ."

//zadanie 3

val listGreaterThanZero: List[Int] => Boolean = (xs: List[Int]) =>
  if xs == Nil then throw new Exception("Empty list !")
  else if xs.head <= 0 then false
  else if xs.tail != Nil then listGreaterThanZero (xs.tail)
  else true

//listGreaterThanZero (Nil)
listGreaterThanZero (List(1)) == true
listGreaterThanZero (List(-1)) == false
listGreaterThanZero (List(1, 2, 0)) == false

//zadanie 4

val factorial: Int => Int = (n: Int) =>
  if n < 0 then throw new Exception("Neagtive argument !")
  else if n == 0 then 1
  else n * factorial (n-1)

//factorial (-1)
factorial (0) == 1
factorial (1) == 1
factorial (3) == 6
