// Gracjan Pasik

// Zadanie 1
def sum_list(list_of_numbers: List[Int]) : Int =
  if (list_of_numbers.isEmpty) 0
  else list_of_numbers.head + sum_list(list_of_numbers.tail)

sum_list(List(9, 1, 2, 3))
sum_list(List())
sum_list(List(-1))

// Zadanie 2
def list_to_one_string(list_of_words: List[String], ending_character: Char) : String =
  if (list_of_words.isEmpty) ending_character.toString
  else if (list_of_words.tail.isEmpty) list_of_words.head + ending_character.toString
  else list_of_words.head + " " + list_to_one_string(list_of_words.tail, ending_character)

list_to_one_string(List("Ala", "ma", "kota"), '.')
list_to_one_string(List(), '?')
list_to_one_string(List("Tomek"), '!')

// Zadanie 3
def are_numbers_greater_than_0(numbers: List[Int]): Boolean =
  if (numbers.isEmpty) true
  else {
    if (numbers.head > 0) are_numbers_greater_than_0(numbers.tail)
    else false
  }

are_numbers_greater_than_0(List())
are_numbers_greater_than_0(List(1, 2, 3, -1))
are_numbers_greater_than_0(List(1, -22, 3, 19))
are_numbers_greater_than_0(List(15, 17, 8, 11, 100))
are_numbers_greater_than_0(List(0, 0, 0, 0))

// Zadanie 4
// założenie: liczba jest >= 0, inaczej silnia nie jest zdefiniowana
def factorial(number: Int): Int =
  def accumulating(number: Int, value: Int): Int =
    if (number == 0) value
    else accumulating(number-1, number * value)
  accumulating(number, 1)


factorial(5)
factorial(8)
factorial(0)
