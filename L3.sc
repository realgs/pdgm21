//o(n^2)
def splitBySign(nums : List[Int]) : List[List[Int]] = {
  def splitBySignIn(numbers: List[Int], positiveOdd: List[Int], negative: List[Int]): List[List[Int]] = {
    if numbers == List() then List(positiveOdd, negative)
    else if numbers.head < 0 then splitBySignIn(numbers.tail, positiveOdd, negative ++ List(numbers.head))
    else if numbers.head > 0 && numbers.head % 2 == 1 then splitBySignIn(numbers.tail, positiveOdd ++ List(numbers.head), negative)
    else splitBySignIn(numbers.tail, positiveOdd, negative)
  }
  splitBySignIn(nums, List(), List())
}

splitBySign(List(1,-22,3,-41,-5,61))
splitBySign(List(12,23,3,4,51))
splitBySign(List(-13,-2,-3,-4,-51))



//o(N)
def lengthOfList[A](list:List[A]): Int = {
    def lengthOfListIter[A](list: List[A],iter:Int):Int={
      if list == List() then iter
      else lengthOfListIter(list.tail,iter+1)
    }
  lengthOfListIter(list,0)
}
lengthOfList(List(1,2,3,4))
lengthOfList(List())
lengthOfList(List("Gorzow","wielkopolski","jest","stolica","poslki"))

//o(n)
def joinLists[A](list1: List[A],list2: List[A]):List[A] = {
    if list1==List() && list2==List() then Nil
    else if list1==List() then list2
    else if list2==List() then list1
    else list1.head::list2.head::joinLists(list1.tail,list2.tail)
}
joinLists(List(5,4,3,2),List(1,2,3,4,5,6))
joinLists(List(5,6),List(1,2,3))
joinLists(List(1,2,3),List(5,6))
