//Monika Jung

val list1 = 1::(-5)::8::4::(-3)::2::1::0::Nil
val list2 = 7::(-7)::9::(-8)::3::Nil
val list3 = 3::(-2)::(-1)::2::Nil
val wList1 = "a"::"b"::"c"::Nil
val wList2 = "s"::"t"::"u"::"w"::"x"::"y"::Nil

//task 1 - assuming zero as positive
def createTwoListsF (list:List[Int]):(List[Int], List[Int])  =
{
  def recCreateTwoListsF (list:List[Int], listA:List[Int], listB:List[Int]):(List[Int], List[Int]) =
    if list == Nil then (listA, listB)
    else if list.head < 0 then recCreateTwoListsF(list.tail, listA:::List(list.head), listB)
    else recCreateTwoListsF (list.tail, listA, listB:::List(list.head))
  recCreateTwoListsF (list, Nil, Nil)
}

createTwoListsF (list1)
createTwoListsF (list2)
createTwoListsF (list3)

//task 2
def findListLengthF[A] (list:List[A]):Int = {
  def recFindListLengthF(list: List[A], resultInt: Int):Int =
    if list == Nil then resultInt
    else recFindListLengthF (list.tail, resultInt + 1)
  recFindListLengthF (list, 0)
}
findListLengthF (list1)
findListLengthF (list2)
findListLengthF (list3)

//task 3
def mergeTwoListsF[A] (listA:List[A], listB:List[A]):List[A] = {
  def recMergeTwoListsF(listA: List[A], listB: List[A], resultList: List[A]): List[A] =
    if listB == Nil then resultList ::: listA
    else if listA == Nil then resultList ::: listB
    else recMergeTwoListsF (listB.tail, listA, resultList ::: List(listB.head))
  recMergeTwoListsF(listA, listB, Nil)
}
mergeTwoListsF (list1, list2)
mergeTwoListsF (list2, list1)
mergeTwoListsF (list2, list3)
mergeTwoListsF (wList1, wList2)
