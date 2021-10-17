//zadanie 1

val listSum: List[Int] => Int = (intList: List[Int]) =>
  if intList == Nil then 0
  else intList.head + listSum (intList.tail)

listSum (Nil) == 0
listSum (List(2)) == 2
listSum (List(-2, 3, -4, 1)) == -2

//zadanie 2

val sentenceFromList: (List[String], String) => String = (stringList: List[String], endString: String) =>
  if stringList == Nil then endString
  else stringList.head + " " + sentenceFromList (stringList.tail, endString)

sentenceFromList (Nil, "") == ""
sentenceFromList (Nil, "!") == "!"
sentenceFromList (List("zadanie", "drugie"), ".") == "zadanie drugie ."

//zadanie 3

val listGreaterThanZero: List[Int] => Boolean = (intList: List[Int]) =>
  if intList == Nil then throw new Exception("Empty list !")
  else if intList.head <= 0 then false
  else if intList.tail != Nil then listGreaterThanZero (intList.tail)
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
