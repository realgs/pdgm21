import scala.annotation.tailrec

// Task 1
val sum: List[Int] => Int = xs =>
  if xs != Nil then xs.head + sum(xs.tail)
  else 0

sum(List(1, 2, 3, 4)) == 10
sum(List(0)) == 0
sum(Nil) == 0


// Task 2
val merge_strings: List[String] => String = xs =>
  val sep = " "
  if xs != Nil then
    if (xs.tail != Nil)
      if(xs.tail.tail != Nil) then xs.head + sep + merge_strings(xs.tail)
      else xs.head + merge_strings(xs.tail)
    else xs.head
  else ""

merge_strings(List("Ala", "ma", "kota", ".")) == "Ala ma kota."
merge_strings(List("Kot", ".")) == "Kot."
merge_strings(Nil) == ""


// Task 3
val are_positive: List[Number] => Boolean = xs2 =>
    if xs2 == Nil then true
    else if (xs2.head.doubleValue() > 0) then are_positive(xs2.tail)
    else false

are_positive(List(1, 2)) == true
are_positive(List(1, 2, 0)) == false
are_positive(Nil) == true


// Task 4
val factorial: Int => Int = x =>
    if x < 0 then throw new Exception("Illegal argument passed!")
    else if x == 0 then 1
    else x * factorial(x - 1)



print_string(string_of_bool(factorial(5) = 120)^"\n");;
factorial(0) == 1
factorial(1) == 1
//factorial(-2)
