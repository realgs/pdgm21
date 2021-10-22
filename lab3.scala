import scala.annotation.tailrec

val splitBySign: (List[Int] => (List[Int], List[Int])) = inputList =>
    def myReverse(inputList: List[Any]): List[Any] =
        @tailrec
        def myReverseTailrec(inputList: List[Any], accumulator: List[Any]): List[Any] =
            if inputList == Nil then accumulator
            else myReverseTailrec(inputList.tail, inputList.head :: accumulator)
        myReverseTailrec(inputList, Nil)
    @tailrec
    def splitBySignTailrec(inputList: List[Int], result: (List[Int], List[Int])): (List[Int], List[Int]) =
        if inputList == Nil then (myReverse(result._1), myReverse(result._2.reverse))
        else if inputList.head < 0 then splitBySignTailrec(inputList.tail, (inputList.head :: result._1, result._2))
        else if inputList.head%2 == 1 then splitBySignTailrec(inputList.tail, (result._1, inputList.head :: result._2))
        else splitBySignTailrec(inputList.tail, result)
    splitBySignTailrec(inputList, (Nil, Nil))

splitBySign(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign(List(1, -1, 2)) == (List(-1), List(1))
splitBySign(Nil) == (Nil, Nil)
splitBySign(List(0, 1, 2, 3, 4, 5)) == (Nil, List(1, 3, 5))

val lengthOfList: (List[Any] => Int) = inputList =>
    @tailrec
    def lengthOfListTailrec(inputList: List[Any], accumulator: Int): Int =
        if inputList == Nil then accumulator
        else lengthOfListTailrec(inputList.tail, accumulator + 1)
    lengthOfListTailrec(inputList, 0)

lengthOfList(List(5, 4, 3, 2)) == 4
lengthOfList(Nil) == 0
lengthOfList(List(List('a', 's', 'd', 'f'))) == 1

val joinLists: ((List[Any], List[Any]) => List[Any]) = (firstInputList, secondInputList) =>
    def myReverse(inputList: List[Any]): List[Any] =
        @tailrec
        def myReverseTailrec(inputList: List[Any], accumulator: List[Any]): List[Any] =
            if inputList == Nil then accumulator
            else myReverseTailrec(inputList.tail, inputList.head :: accumulator)
        myReverseTailrec(inputList, Nil)
    @tailrec
    def joinListsTailrec(firstInputList: List[Any], secondInputList: List[Any], accumulator: List[Any], fromFirstList: Boolean): List[Any] =
        (firstInputList, secondInputList) match
            case (_, Nil) => myReverse(accumulator) ::: firstInputList
            case (Nil, _) => myReverse(accumulator) ::: secondInputList
            case _ => if fromFirstList then joinListsTailrec(firstInputList.tail, secondInputList, firstInputList.head :: accumulator, !fromFirstList)
                      else joinListsTailrec(firstInputList, secondInputList.tail, secondInputList.head :: accumulator, !fromFirstList)
    joinListsTailrec(firstInputList, secondInputList, Nil, true)

joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(Nil, Nil) == Nil
joinLists(Nil, List(1, 2, 8, 9)) == List(1, 2, 8, 9)
joinLists(List(3, 4, 6, 7), Nil) == List(3, 4, 6, 7)
