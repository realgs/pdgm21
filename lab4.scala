import scala.annotation.tailrec

/**
 * Metody pomocnicze
 */
def myConcat(first: List[Any], second: List[Any]): List[Any] =  // rekursja nieogonowa, zlozonosc pamieciowa i obliczeniowa liniowa wzgledem first
    if first == Nil then second
    else first.head :: myConcat(first.tail, second)

@tailrec
def isSubstring(word: String, substr: String, i: Int, wordOffset: Int): Boolean =  // rekursja ogonowa, zlozonosc stala pamieciowa i kwadratowa obliczeniowa (n*n)/2 wzgledem substr
        if substr == "" then false
        else if i == substr.length() then true
        else if i == word.length() - wordOffset then false
        else if word(i+wordOffset) != substr(i) then 
            if substr.length() + wordOffset == word.length() then false
            else isSubstring(word, substr, 0, wordOffset+1)
        else isSubstring(word, substr, i+1, wordOffset)
/*
    W  O  R  D  X  X    offset = 0
       W  O  R  D  X    offset = 1
          W  O  R  D    offset = 2
             W  O  R   ...itd
*/

@tailrec
def isSubstringOfAny(word: String, substrs: List[String]): Boolean =  // rekursja ogonowa, zlozonosc stala pamieciowa i liniowa obliczeniowa wzgledem substrs
        if substrs == Nil then false
        else if isSubstring(word, substrs.head, 0, 0) then true
        else isSubstringOfAny(word, substrs.tail)

def myReverse[A](inputList: List[A]): List[A] =  // rekursja ogonowa, zlozonosc stala pamieciowa i liniowa obliczeniowa
        @tailrec
        def myReverseTailrec(inputList: List[A], accumulator: List[A]): List[A] =
            if inputList == Nil then accumulator
            else myReverseTailrec(inputList.tail, inputList.head :: accumulator)
        myReverseTailrec(inputList, Nil)

/** 
 * Zadanie 1, jedna fraza, rekursja nieogonowa
 */
val find: ((List[String], String) => List[String]) = (words, phrase) =>  // zlozonosc obliczeniowa n^3 (n^2 dla isSubstring * n dla listy words), pamieciowa liniowa
    if words == Nil then Nil
    else if isSubstring(words.head, phrase, 0, 0) then words.head :: find(words.tail, phrase)
    else find(words.tail, phrase)

find (List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210")
find (List("kot", "koc", "ko", "koty", " kot", "pokot"), "kot") == List("kot", "koty", " kot", "pokot")
find (Nil, "pies") == Nil
find (List("aaa", "bbb", "ccc", "", "   "), "") == List("aaa", "bbb", "ccc", "", "   ")

/** 
 * Zadanie 1, jedna fraza, rekursja ogonowa
 */
val findTailrec: ((List[String], String) => List[String]) = (words, phrase) =>  // zlozonosc obliczeniowa n^3 (n^2 dla isSubstring * n dla listy words), pamieciowa stala
    @tailrec
    def findTailrecHelper(words: List[String], phrase: String, accumulator: List[String]): List[String] =
        if words == Nil then myReverse(accumulator)
        else if isSubstring(words.head, phrase, 0, 0) then findTailrecHelper(words.tail, phrase, words.head :: accumulator)
        else findTailrecHelper(words.tail, phrase, accumulator)
    findTailrecHelper(words, phrase, Nil)

findTailrec (List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168") == List("iindex0168202", "iindex0168211", "iindex0168210")
findTailrec (List("kot", "koc", "ko", "koty", " kot", "pokot"), "kot") == List("kot", "koty", " kot", "pokot")
findTailrec (Nil, "pies") == Nil
findTailrec (List("aaa", "bbb", "ccc", "", "   "), "") == List("aaa", "bbb", "ccc", "", "   ")

/** 
 * Zadanie 1, wiele fraz, rekursja nieogonowa
 */
val findMany: ((List[String], List[String]) => List[String]) = (words, phrases) =>  // zlozonosc obliczeniowa n^4 (n^3 dla isSubstringOfAny * n dla listy words), pamieciowa liniowa
    if words == Nil then Nil
    else if isSubstringOfAny(words.head, phrases) then words.head :: findMany(words.tail, phrases)
    else findMany(words.tail, phrases)

findMany (List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168", "224")) == List("iindex0168202", "iindex0168211", "iindex0168210", "index0169224")
findMany (List("kot", "koc", "ko", "koty", " kot", "pokot", "kotopies", "pies", "ewdxweq", "pie"), List("kot", "pies")) == List("kot", "koty", " kot", "pokot", "kotopies", "pies")
findMany (Nil, List("pies")) == Nil
findMany (Nil, Nil) == Nil
findMany (List("aaa", "bbb", "ccc"), Nil) == Nil

/** 
 * Zadanie 1, wiele fraz, rekursja ogonowa
 */
 val findManyTailrec: ((List[String], List[String]) => List[String]) = (words, phrases) =>  // zlozonosc obliczeniowa n^4 (n^3 dla isSubstringOfAny * n dla listy words), pamieciowa stala
    @tailrec
    def findManyTailrecHelper(words: List[String], phrases: List[String], accumulator: List[String]): List[String] =
        if words == Nil then myReverse(accumulator)
        else if isSubstringOfAny(words.head, phrases) then findManyTailrecHelper(words.tail, phrases, words.head :: accumulator)
        else findManyTailrecHelper(words.tail, phrases, accumulator)
    findManyTailrecHelper(words, phrases, Nil)

findManyTailrec (List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), List("index0168", "224")) == List("iindex0168202", "iindex0168211", "iindex0168210", "index0169224")
findManyTailrec (List("kot", "koc", "ko", "koty", " kot", "pokot", "kotopies", "pies", "ewdxweq", "pie"), List("kot", "pies")) == List("kot", "koty", " kot", "pokot", "kotopies", "pies")
findManyTailrec (Nil, List("pies")) == Nil
findManyTailrec (Nil, Nil) == Nil
findManyTailrec (List("aaa", "bbb", "ccc"), Nil) == Nil

/** 
 * Zadanie 2, rekursja nieogonowa
 */
val joinLists: ((List[Any], List[Any], List[Any]) => List[Any]) = (firstList, secondList, thirdList) =>  // zlozonosc obliczeniowa liniowa wzgledem first + second, pamieciowa tak samo
    if firstList == Nil then
        if secondList == Nil then thirdList
        else secondList.head :: joinLists(firstList, secondList.tail, thirdList)
    else firstList.head :: joinLists(firstList.tail, secondList, thirdList)

joinLists (List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9)
joinLists (Nil, List('p', 'q', 'r'), List('s', 't', 'u', 'w')) == List('p', 'q', 'r', 's', 't', 'u', 'w')
joinLists (Nil, Nil, Nil) == Nil

/** 
 * Zadanie 2, rekursja ogonowa
 */
val joinListsTailrec: ((List[Any], List[Any], List[Any]) => List[Any]) = (firstList, secondList, thirdList) =>  // zlozonosc obliczeniowa liniowa wzgledem first + second + third, pamieciowa stala
    @tailrec
    def joinListsTailrecHelper(firstList: List[Any], secondList: List[Any], thirdList: List[Any], accumulator: List[Any]): List[Any] =
        if firstList == Nil then
            if secondList == Nil then myReverse(myConcat(myReverse(thirdList), accumulator))
            else joinListsTailrecHelper(firstList, secondList.tail, thirdList, secondList.head :: accumulator)
        else joinListsTailrecHelper(firstList.tail, secondList, thirdList, firstList.head :: accumulator)
    joinListsTailrecHelper(firstList, secondList, thirdList, Nil)

joinListsTailrec (List(5, 4, 3, 2), List(1, 0), List(9)) == List(5, 4, 3, 2, 1, 0, 9)
joinListsTailrec (Nil, List('p', 'q', 'r'), List('s', 't', 'u', 'w')) == List('p', 'q', 'r', 's', 't', 'u', 'w')
joinListsTailrec (Nil, Nil, Nil) == Nil