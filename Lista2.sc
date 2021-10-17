//Lista 2 (Scala)

//zadanie 1
def list_sum (li: List[Int]): Int =
{
  if li == Nil then 0
  else li.head + list_sum(li.tail)
}

list_sum(List(1, 2, 3)) == 6
list_sum(List(12, 12, 13, 14, 15)) == 66
list_sum(List(-5, -4, -3, -2, -1, 0, 1, 2, 3, 4, 5)) == 0
list_sum(List(-20)) == -20
list_sum(List()) == 0

//zadanie 2
def make_sentence (li: List[String], s: String): String =
{
  val operator = " "

  if li == Nil then s
  else if li.tail == Nil then li.head + s
  else li.head + operator + make_sentence(li.tail, s)
}

make_sentence(List("Ala", "ma", "kota"), ".") == "Ala ma kota."
make_sentence(List("Czy", "program", "działa", "poprawnie"), "?") == "Czy program działa poprawnie?"
make_sentence(List("Aaa"), "!") == "Aaa!"
make_sentence(List(), "?") == "?"

//zadanie 3
def greater_than_zero (li: List[Double]): Boolean =
  {
    if li == Nil then false
    else if li.tail == Nil then li.head>0
    else li.head>0 && greater_than_zero(li.tail)
  }

greater_than_zero(List(1, 2, 3)) == true
greater_than_zero(List(0, 1, 2, 3)) == false
greater_than_zero(List(1.2, 2.3, 3.4, 4.5)) == true
greater_than_zero(List(-5.3, 5, 25, 4, 3)) == false
greater_than_zero(List(10000)) == true
greater_than_zero(List(0)) == false
greater_than_zero(List()) == false

//zadanie 4
def factorial (n: Int): Int =
  {
    if n<0 then throw new Exception("Ujemny argument")
    else if n==0 then 1
    else n*factorial(n-1)
  }

factorial(0) == 1
factorial(1) == 1
factorial(2) == 2
factorial(5) == 120
factorial(10) == 3628800
//factorial(-1)

