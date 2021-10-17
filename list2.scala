// Kacper Wójcicki


// 1
val sumOfListNums: (List[Int]) => Int = xs => {
    if xs == List() then 0
    else xs.head + sumOfListNums(xs.tail)
}

sumOfListNums(List(1,2,3)) == 6
sumOfListNums(List()) == 0
sumOfListNums(List(-5, -3, -10)) == -18

// 2
def makeSentence(words : List[String], endChar : String) : String = {
    def makeSentenceIn(words : List[String], endChar : String, result : String) : String = {
        if words == List() then throw new Exception("Sentence end char not found in words list!")
        else if words.head == endChar then result + endChar
        else if result == "" then makeSentenceIn(words.tail, endChar, words.head)
        else makeSentenceIn(words.tail, endChar, result + " " + words.head)
    }
    if words == List() then ""
    else makeSentenceIn(words, endChar, "")
}

makeSentence(List("Hello", "World", "!"), "!") == "Hello World!"
makeSentence(List(), "!") == ""
makeSentence(List("!"), "!") == "!"

// 3
val isBiggerThanZero: (List[Int]) => Boolean = xs => {
    if xs == List() then true
    else if xs.head <= 0 then false
    else isBiggerThanZero(xs.tail)
}

isBiggerThanZero(List(1,2,543,432)) == true
isBiggerThanZero(List(0)) == false
isBiggerThanZero(List()) == true

// 4
val myFactorial: (Int) => Int = number => {
    if number < 0 then throw new Exception("Invalid argument")
    else if number == 1 || number == 0 then 1
    else number * myFactorial(number - 1)
}

myFactorial(6) == 720
myFactorial(1) == 1
myFactorial(3) == 6
myFactorial(0) == 1
