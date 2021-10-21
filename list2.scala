// first task

// mogę coś źle rozumieć ale chciałem zrobić jedno zadanie na takie dwa sposoby żeby
// może na rozmowie dopytać dokładnie o rekursję ogonową

val sumListElements: List[Int] => Int = list =>  // zlozonosc O(2n) = O(n)
  if list == Nil then 0
  else list.head + sumListElements(list.tail)

def sumListElementsTail(list: List[Int]): Int =  // zlozonosc O(n)
  def sumListElementsTailIter(list: List[Int], value: Int): Int =
    if list == Nil then value
    else sumListElementsTailIter(list.tail, value + list.head)
  sumListElementsTailIter(list, 0)


sumListElements(List(0, 3, 5)) == 8
sumListElements(List()) == 0
sumListElements(List(2, -3, 5, -4, -2)) == -2

sumListElementsTail(List(0, 3, 5)) == 8
sumListElementsTail(List()) == 0
sumListElementsTail(List(2, -3, 5, -4, -2)) == -2

// second task

val createString: (List[String], String) => String = (list, endingChar) =>
  if list == Nil then endingChar
  else if list.tail == Nil then list.head + endingChar
  else list.head + " " + createString(list.tail, endingChar)


createString(List("Scala", ">", "Ocaml"), "!") == "Scala > Ocaml!"
createString(List(), "!") == "!"
createString(List("!"), "") == "!"

// third task
// zalozylem ze jesli lista jest pusta na wejsciu to zwracam true

val checkListPositivity: List[Int] => Boolean = list =>
  if list == Nil then true
  else if list.head < 0 then false
  else checkListPositivity(list.tail)

checkListPositivity(List()) == true
checkListPositivity(List(2, 3, -4)) == false
checkListPositivity(List(2, 3, 5)) == true

// fourth task

val factorial: Int => Int = num =>
  if num < 0 then throw new Exception(s"Ujemny argument silni: $num!")
  else if num == 0 then 1
  else if num == 1 then 1
  else num * factorial(num - 1)

factorial(5) == 120
factorial(0) == 1
factorial(1) == 1
// factorial(-1) rzuca wyjątkiem - ujemny argument!
