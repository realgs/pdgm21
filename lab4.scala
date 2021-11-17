object Main {

    def isPrefix(string: String, prefix: String): Boolean = {
        if string != "" && prefix != "" then
            if string.head == prefix.head then isPrefix(string.tail, prefix.tail)
            else false
        else (string, prefix) match {
            case (_, "") => true
            case ("", _) => false
        }
    }

    def isSubstring(string: String, pattern: String): Boolean = {
        if isPrefix(string, pattern) then true
        else if string == "" then false
        else isSubstring(string.tail, pattern)
    }

    def isSubstringN(string: String, patterns: List[String]): Boolean = {
        def iter(patterns: List[String]): Boolean = {
            if patterns == Nil then false
            else if isSubstring(string, patterns.head)
                then true
                else iter(patterns.tail)
        }
        iter(patterns)
    }

    def reverse[A](list: List[A]): List[A] = {
        def iter(list: List[A], acc: List[A]): List[A] = {
            if list == Nil then acc
            else iter(list.tail, list.head :: acc)
        }
        iter(list, Nil)
    }

    def findRec(list: List[String], phrase: String): List[String] = {
        if list == Nil then Nil
        else if isSubstring(list.head, phrase)
            then list.head :: findRec(list.tail, phrase)
            else findRec(list.tail, phrase)
    }

    def findTailRec(list: List[String], phrase: String): List[String] = {
        def iter(list: List[String], acc: List[String]): List[String] = {
            if list == Nil then reverse(acc)
            else if isSubstring(list.head, phrase)
                then iter(list.tail, list.head :: acc)
                else iter(list.tail, acc)
        }
        iter(list, Nil)
    }

    def findN(list: List[String], phrases: List[String]): List[String] = {
        def iter(list: List[String], acc: List[String]): List[String] = {
            if list == Nil then reverse(acc)
            else if isSubstringN(list.head, phrases)
                then iter(list.tail, list.head :: acc)
                else iter(list.tail, acc)
        }
        iter(list, Nil)
    }

    def joinListsRec[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
        if list1 != Nil then list1.head :: joinListsRec(list1.tail, list2, list3)
        else if list2 != Nil then list2.head :: joinListsRec(Nil, list2.tail, list3)
        else list3
    }

    def joinListsTailRec[A](list1: List[A], list2: List[A], list3: List[A]): List[A] = {
        def reverse(list: List[A], acc: List[A]): List[A] = {
            if list != Nil then reverse(list.tail, list.head :: acc)
            else acc
        }
        def iter(l1: List[A], l2: List[A], l3: List[A], acc: List[A]): List[A] = {
            if l1 != Nil then iter(l1.tail, l2, l3, l1.head :: acc)
            else if l2 != Nil then iter(Nil, l2.tail, l3, l2.head :: acc)
            else if l3 != Nil then iter(Nil, Nil, l3.tail, l3.head :: acc)
            else reverse(acc, Nil)
        }
        iter(list1, list2, list3, Nil)
    }

    def main(args: List[String]): Unit = {
        val result1 = findRec(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168")
        println(result1)
        val result2 = findTailRec(List("index0169", "iindex0168202", "iindex0168211", "iindex0168210", "iindex0169222", "index0169224"), "index0168")
        println(result2)
        val result3 = joinListsRec(List(5, 4, 3, 2), List(1, 0), List(9))
        println(result3)
        val result4 = joinListsTailRec(List(5, 4, 3, 2), List(1, 0), List(9))
        println(result4)
        val result5 = findN(List("aaba", "abaa", "aba", "ab", "ba", "bb"), List("c", "ab", "bb"))
        println(result5)
    }
}
