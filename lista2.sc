///Dawid Krutul

/// Zad 1
def count_sum[A](numbers: List[Int]): Int = {
    if(numbers == Nil) 0
    else numbers.head + count_sum(numbers.tail)

}

count_sum(List(1,2,3,4)) == 10
count_sum(List()) == 0
count_sum(List(-3,-2,-1,15)) == 9

/// Zad 2
val space = " "

def create_sentence[A](words: List[String], dot: String): String = {
  if (words == Nil) dot
    else
      if (words.tail == Nil) words.head.concat(create_sentence(words.tail, dot))
      else words.head.concat(space.concat(create_sentence(words.tail, dot)))
}
create_sentence(List("ala","ma","kota"),".")

/// Zad 3
def natural_numbers[A](numbers: List[Int]): Boolean = {
    if(numbers == Nil) true
      else
          if(numbers.head < 0) false
          else natural_numbers(numbers.tail)
}

natural_numbers(List(1,2,3)) == true
natural_numbers(List(0,5,3,3,3,1,0)) == true
natural_numbers(List(-1,1,3,3,3)) == false

/// Zad 4
def factorial[A](n: Int): Int = {
  if(n == 0) 1
    else n * factorial(n - 1)
}

factorial(0) == 1
factorial(1) == 1
factorial(2) == 2
factorial(3) == 6
factorial(4) == 24

