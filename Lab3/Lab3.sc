def reverseList [A] (list: List[A]): List[A] =
    def reverseHelper(list: List[A], resultList: List[A]): List[A]=
        if list==Nil then resultList
        else reverseHelper(list.tail, list.head::resultList)
    reverseHelper(list, List())

def splitBySign(list:List[Int]): (List[Int], List[Int]) =
    def splitBySignIn(number : List[Int], list1 : List[Int], list2 : List[Int]) : (List[Int], List[Int]) =
        if number != Nil then (number.head < 0, number.head%2 != 0) match
            case(true, _) => splitBySignIn(number.tail,number.head::list1, list2)
            case(false, true) => splitBySignIn(number.tail, list1, number.head::list2 )
            case(_ ,_ ) => splitBySignIn(number.tail, list1, list2)
        else (reverseList(list1), reverseList(list2))
    splitBySignIn(list, List(), List())






def listLength[A](xs: List[A]): Int =
  if (xs != Nil) then
    1 + listLength(xs.tail)
  else 0

listLength(List(1, 3, 2)) == 3
listLength(List("msms", "qwqwqw", "pool", "asdopkasdo")) == 4
listLength(List()) == 0

def joinLists[A](list1: List[A], list2: List[A]): List[A] =
    (list1, list2) match
        case (Nil, Nil) => Nil
        case (Nil, head2 :: tail2) => head2 :: joinLists(list1, tail2)
        case (head1::tail1, Nil) => head1 :: joinLists(tail1, list2)
        case (head1::tail1, head2::tail2) => head1 :: head2 :: joinLists(tail1, tail2)

joinLists(List(), List()) == List()
joinLists(List(5,4,3,2),List(1,2,3,4,5,6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)

