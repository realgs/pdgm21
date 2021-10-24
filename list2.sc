/* task 1 */

def reverse(list: List[Int]): List[Int] =
  def rev(orginalList: List[Int], newList: List[Int]): List[Int] =
    (orginalList) match
      case(Nil) => newList
      case(h :: t) => rev(t, h :: newList)
  rev(list, List())


def splitBySign2(list: List[Int]): (List[Int], List[Int]) =
  if list == Nil then return (Nil, Nil) else {
    def split(validateNumber: (number: Int) => Boolean, orginalList: List[Int], newList: List[Int]): List[Int] =
      (orginalList) match
        case (Nil) => reverse(newList)
        case (h :: t) => if validateNumber(h) then split(validateNumber, t, h :: newList) else split(validateNumber, t, newList)
    (split((x: Int) => x < 0, list, List()), split((x: Int) => x > 0 && x%2 == 1, list, List()))
  }

splitBySign2(List(-3, -6, 7, -9, 13)) == (List(-3, -6, -9), List(7, 13))
splitBySign2(List(1, -2, 8, -1, 9, 3, -4, 6)) == (List(-2, -1, -4), List(1, 9, 3))
splitBySign2(Nil) == (Nil, Nil)
splitBySign2(List(2)) == (List(), List())


/* task 2 */

def listLength[A](list: List[A]): Int =
  if list == Nil then 0 else 1 + listLength(list.tail)

listLength(Nil) == 0
listLength(List()) == 0
listLength(List(2)) == 1
listLength(List(-5, 8, 1)) == 3


/* task 3 */

def joinLists(list1: List[Int], list2: List[Int]): List[Int] =
  (list1, list2) match
    case(Nil, Nil) => Nil
    case(head::tail, Nil) => list1
    case(Nil, head::tail) => list2
    case(h1::t1, h2::t2) => h1:: h2 :: joinLists(t1, t2)


joinLists(List(5, 4, 3, 2), List(1, 2, 3, 4, 5, 6)) == List(5, 1, 4, 2, 3, 3, 2, 4, 5, 6)
joinLists(Nil, Nil) == Nil
joinLists(Nil, List(1, 2)) == List(1, 2)
joinLists(List(1, 6), Nil) == List(1, 6)
joinLists(List('a', 'b'), List('c')) == List('a', 'c', 'b')
