import scala.annotation.tailrec

object Lista4 {

    // Złożoność obliczeniowa O(n), gdzie n to długość listy wejsciowej
    // Złożoność pamięciowa O(1)
    def reverse[T](list: List[T]): List[T] =
        @tailrec
        def reverseTail[T](list: List[T], reversed: List[T]): List[T] =
            list match
                case Nil => reversed
                case _ => reverseTail(list.tail, list.head :: reversed)
        return reverseTail(list, List())

    // Złożoność obliczeniowa O(n), gdzie n to długość prefixu
    // Złożoność pamięciowa O(1)
    @tailrec
    def startsWith(string: String, prefix: String): Boolean =
        (string, prefix) match
            case (_, "") => true
            case ("", _) => false
            case (_, _) =>
                if string.head == prefix.head then startsWith(string.tail, prefix.tail)
                else false

    // Złożoność obliczeniowa O(n * m), gdzie n to długość ciągu znaków, m to długość sprawdzanego podciągu
    // Złożoność pamięciowa O(1)
    @tailrec
    def contains(string: String, substring: String): Boolean =
        (string, substring) match
            case (_, "") => false
            case ("", _) => false
            case (_, _) =>
                if startsWith(string, substring) then true
                else contains(string.tail, substring)

    // Złożoność obliczeniowa O(n * m), gdzie n to długość ciągu znaków, m to suma długości napisów z listy podciągów
    // Złożoność pamięciowa O(1)
    @tailrec
    def containsAny(string: String, substrings: List[String]): Boolean =
        if substrings == Nil then false
        else
            if contains(string, substrings.head) then true
            else containsAny(string, substrings.tail)

    // Złożoność obliczeniowa O(n * m), gdzie n to długość podciągu, m to sumaryczna długość napisów z listy ciągów
    // Złożoność pamięciowa O(1)
    def findSubstringedTailrec(list: List[String], substring: String): List[String] =
        @tailrec
        def findSubstringedTail(list: List[String], substring: String, result: List[String]): List[String] =
            list match
                case Nil => result
                case _ =>
                    if contains(list.head, substring) then findSubstringedTail(list.tail, substring, list.head :: result)
                    else findSubstringedTail(list.tail, substring, result)
        return reverse(findSubstringedTail(list, substring, Nil))

    // Złożoność obliczeniowa O(n * m), gdzie n to długość podciągu, m to sumaryczna długość napisów z listy słów
    // Złożoność pamięciowa O(n)
    def findSubstringed(list: List[String], substring: String): List[String] =
        list match
            case Nil => Nil
            case _ =>
                if contains(list.head, substring) then list.head :: findSubstringed(list.tail, substring)
                else findSubstringed(list.tail, substring)

    // Złożoność obliczeniowa O(n * m), gdzie n to sumaryczna długość napisów z listy podciągów, m to sumaryczna długość napisów z listy ciągów
    // Złożoność pamięciowa O(1)
    def findAllSubstringedTailrec(list: List[String], substrings: List[String]): List[String] =
        @tailrec
        def findAllSubstringedTail(list: List[String], substrings: List[String], result: List[String]): List[String] =
            (list, substrings) match
                case (Nil, _) => result
                case (_, Nil) => result
                case (_, _) =>
                    if containsAny(list.head, substrings) then findAllSubstringedTail(list.tail, substrings, list.head :: result)
                    else findAllSubstringedTail(list.tail, substrings, result)
        return reverse(findAllSubstringedTail(list, substrings, Nil))

    // Złożoność obliczeniowa O(n * m), gdzie n to sumaryczna długość napisów z listy podciągów, m to sumaryczna długość napisów z listy ciągów
    // Złożoność pamięciowa O(n)
    def findAllSubstringed(list: List[String], substrings: List[String]): List[String] =
        (list, substrings) match
            case (Nil, _) => Nil
            case (_, Nil) => Nil
            case (_, _) =>
                if containsAny(list.head, substrings) then list.head :: findAllSubstringed(list.tail, substrings)
                else findAllSubstringed(list.tail, substrings)

    // Złożoność obliczeniowa O(n), gdzie n to suma długości wszystkich list
    // Złożoność pamięciowa O(1)
    def joinThreeListsTailrec[T] (listA: List[T], listB: List[T], listC: List[T]): List[T] =
        @tailrec
        def joinThreeListsTail(listA: List[T], listB: List[T], listC: List[T], mergedList: List[T]): List[T] =
            (listA, listB, listC) match
                case (Nil, Nil, Nil) => reverse(mergedList)
                case (head :: tail, _, _) => joinThreeListsTail(tail, listB, listC, head :: mergedList)
                case (_, head :: tail, _) => joinThreeListsTail(Nil, tail, listC, head :: mergedList)
                case (_, _, head :: tail) => joinThreeListsTail(Nil, Nil, tail, head :: mergedList)
        return joinThreeListsTail(listA, listB, listC, Nil)

    // Złożoność obliczeniowa O(n), gdzie n to suma długości wszystkich list
    // Złożoność pamięciowa O(n)
    def joinThreeLists[T] (listA: List[T], listB: List[T], listC: List[T]): List[T] =
        (listA, listB, listC) match
            case (Nil, Nil, Nil) => Nil
            case (head :: tail, _, _) => head :: joinThreeLists(tail, listB, listC)
            case (_, head :: tail, _) => head :: joinThreeLists(Nil, tail, listC)
            case (_, _, head :: tail) => head :: joinThreeLists(Nil, Nil, tail)

    def main(args: Array[String]): Unit = {
        // Tests from task
        println(findSubstringed(List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222",
            "index0169224"), ""))

        println(findAllSubstringed(List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222",
            "index0169224"), List("")))

        println(findSubstringed(List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222",
            "index0169224"), "index0168") == List("iindex016802", "iindex0168211", "iindex0168210"))

        println(findAllSubstringed(List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222",
            "index0169224"), List("index0168", "index0169")) ==
            List("index0169", "iindex016802", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"))

        // tests for findSubstringedTailrec
        println("\nTests for findSubstringedTailrec:")
        println(findSubstringedTailrec(List("abcd", "abcdddaa", "aa", "aabcda", "bcdaabcdaa"), "aa"))
        println(findSubstringedTailrec(List("abcd", "abcdddaa", "aa", "aabcda", "bcdaabcdaa"), "bcd"))

        // tests for findSubstringed
        println("\nTests for findSubstringed:")
        println(findSubstringed(List("abcd", "abcdddaa", "aa", "aabcda", "bcdaabcdaa"), "aa"))
        println(findSubstringed(List("abcd", "abcdddaa", "aa", "aabcda", "bcdaabcdaa"), "bcd"))

        // tests for findAllSubstringedTailrec
        println("\nTests for findAllSubstringedTailrec:")
        println(findAllSubstringedTailrec(List("abcd", "abcdddaa", "aa", "aabcda", "bcdaabcdaa"), List("aa")))
        println(findAllSubstringedTailrec(List("abcd", "abcdddaa", "aa", "aabcda", "bcdaabcdaa"), List("aa", "bcd")))
        println(findAllSubstringedTailrec(List("abcd", "abcdddaa", "aa", "aabcda", "bcdaabcdaa"), List("aa", "bcd", "abc")))

        // tests for findAllSubstringed
        println("\nTests for findAllSubstringed:")
        println(findAllSubstringed(List("abcd", "abcdddaa", "aa", "aabcda", "bcdaabcdaa"), List("aa")))
        println(findAllSubstringed(List("abcd", "abcdddaa", "aa", "aabcda", "bcdaabcdaa"), List("aa", "bcd")))
        println(findAllSubstringed(List("abcd", "abcdddaa", "aa", "aabcda", "bcdaabcdaa"), List("aa", "bcd", "abc")))

        // tests for joinThreeListsTailrec
        println("\nTests for joinThreeListsTailrec:")
        println(joinThreeListsTailrec(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)))
        println(joinThreeListsTailrec(List(1, 2, 3), List(), List(7, 8, 9, 10)))
        println(joinThreeListsTailrec(List(1, 2, 3), List(4, 5, 6), List()))
        println(joinThreeListsTailrec(List(), List(), List()))
        println(joinThreeListsTailrec(List(), List(), List(1, 2, 3)))
        println(joinThreeListsTailrec(List(1, 2, 3), List(), List()))
        println(joinThreeListsTailrec(List(), List(4, 5, 6), List()))

        // tests for joinThreeLists
        println("\nTests for joinThreeLists:")
        println(joinThreeLists(List("a", "b", "c"), List("d", "e", "f"), List("g", "h", "i")))
        println(joinThreeLists(List("a", "b", "c"), List("d", "e", "f"), List()))
        println(joinThreeLists(List(), List("d", "e", "f"), List("g", "h", "i")))
        println(joinThreeLists(List(), List(), List("g", "h", "i")))
        println(joinThreeLists(List("a", "b", "c"), List(), List("g", "h", "i")))
    }
}
