val sumList: (List[Int] => Int) = intList =>
    def sumListIteration(intList: List[Int], accumulator: Int): Int =
        if intList == Nil then accumulator
        else sumListIteration(intList.tail, accumulator + intList.head)
    sumListIteration(intList, 0)

sumList(List(1, 3, 5, 7, 9)) == 25
sumList(List()) == 0
sumList(List(-2, -4, -6, -8, -10)) == -30

val toSentence: ((List[String], String) => String) = (stringList, stop) =>
    def toSentenceIteration(stringList: List[String], stop: String, result: String): String =
        if stringList == Nil then result + stop
        else toSentenceIteration(stringList.tail, stop, result + (if result == "" then "" else " ") + stringList.head)
    toSentenceIteration(stringList, stop, "")

toSentence(List("Ala", "ma", "kota"), ".") == "Ala ma kota."
toSentence(List("Czy", "Ala", "ma", "kota"), "?") == "Czy Ala ma kota?"
toSentence(List[String](), "") == ""
toSentence(List("Zdanie", "bez", "kropki"), "") == "Zdanie bez kropki"
toSentence(List[String](), "!") == "!"

val areAllPositive: (List[Int] => Boolean) = intList =>
    if intList == Nil then true
    else if intList.head <= 0 then false
    else areAllPositive(intList.tail)

areAllPositive(List(1, 4, 10, 15)) == true
areAllPositive(List(5, 0, 22)) == false
areAllPositive(List()) == true

val factorial: (Int => Int) = number =>
    def factorialIteration(number: Int, accumulator: Int): Int =
        if number < 0 then throw new Exception("Number cannot be negative")
        else if number == 0 || number == 1 then accumulator
        else factorialIteration(number - 1, accumulator*number)
    factorialIteration(number, 1)

factorial(0) == 1
factorial(1) == 1
factorial(5) == 120
