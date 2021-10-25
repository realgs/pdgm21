// Kacper Wójcicki

import scala.annotation.tailrec

// 1
def splitBySign(nums : List[Int]) : (List[Int], List[Int]) = 
    def splitBySignIn(nums : List[Int], negativeNums : List[Int], postiveOddNums : List[Int]) : (List[Int], List[Int]) = 
        if nums == List() then (negativeNums, postiveOddNums) 
        else if nums.head < 0 then splitBySignIn(nums.tail, negativeNums ::: List(nums.head), postiveOddNums)
        else if ((nums.head > 0) && (nums.head % 2 != 0)) then splitBySignIn(nums.tail, negativeNums, postiveOddNums ::: List(nums.head))
        else splitBySignIn(nums.tail, negativeNums, postiveOddNums)
    splitBySignIn(nums, List(), List())
        
splitBySign(List(-3,-6,7,-9,13)) == (List(-3,-6,-9), List(7, 13))
splitBySign(List(-3,-6,0,8,7,-9,13)) == (List(-3,-6,-9), List(7, 13))
splitBySign(List()) == (List(), List())

// 2
def lenghtOfList[A](list : List[A]) : Int = {
    @tailrec
    def lenghtOfListTailRec(list : List[A], i: Int) : Int = {
        if list == List() then i
        else lenghtOfListTailRec(list.tail, i + 1)
    }
    lenghtOfListTailRec(list, 0)
}

lenghtOfList(List(1,2,3)) == 3
lenghtOfList(List("Lech", "Poznan", "Mistrzem", "Polski")) == 4
lenghtOfList(List()) == 0

// 3
def joinLists[A](list1 : List[A], list2 : List[A]) : List[A] = {
    if list1 == List() then list2
    else if list2 == List() then list1
    else list1.head :: list2.head :: joinLists(list1.tail, list2.tail)
}

joinLists(List(5,4,3,2), List(1,2,3,4,5,6)) == List(5,1,4,2,3,3,2,4,5,6)
