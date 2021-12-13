import scala.annotation.tailrec

object L6 {

  // 1
  def pickEachNElem[A](elements: LazyList[A], n : Int, m: Int) : LazyList[A] = {
    @tailrec
    def pickEachNElemHelper(elements: LazyList[A], resultList : LazyList[A], index : Int, counter : Int) : LazyList[A] = {
      if index == m || elements == LazyList() then resultList
      else {
        if counter == n then pickEachNElemHelper(elements.tail, elements.head #:: resultList, index + 1, 1)
        else pickEachNElemHelper(elements.tail, resultList, index + 1, counter + 1)
      }
    }
    if n < 1 then throw IllegalArgumentException("n has to be > 0 when picking each n element")
    if m < n then LazyList()
    else pickEachNElemHelper(elements.tail, LazyList(elements.head), 1, 1).reverse
  }

  // 2
  def lazyExecute(list1 : LazyList[Int], list2 : LazyList[Int], operation : Char) : LazyList[Int] = {
    @tailrec
    def lazyExecuteHelper(list1 : LazyList[Int], list2 : LazyList[Int], resultList : LazyList[Int]) : LazyList[Int] = {
      (list1, list2) match {
        case (LazyList(), LazyList()) => resultList
        case (LazyList(), _) => resultList.appendedAll(list2.toList)
        case (_, LazyList()) => resultList.appendedAll(list1.toList)
        case (_, _) => {
          operation match {
            case '+' => lazyExecuteHelper(list1.tail, list2.tail, (list1.head + list2.head) #:: resultList)
            case '-' => lazyExecuteHelper(list1.tail, list2.tail, (list1.head - list2.head) #:: resultList)
            case '*' => lazyExecuteHelper(list1.tail, list2.tail, (list1.head * list2.head) #:: resultList)
            case '/' => lazyExecuteHelper(list1.tail, list2.tail, (list1.head / list2.head) #:: resultList)
            case _ => throw IllegalArgumentException("Unknown operation")
          }
        }
      }
    }
    lazyExecuteHelper(list1, list2, LazyList()).reverse
  }

  // 3
  def duplicate[A](list : List[A], duplicateNumberList : List[Int]) : List[A] = {
    @tailrec
    def duplicateHelper(inCounter: Int, element: A, tail: List[A]): List[A] =
      inCounter match
        case 0 => tail
        case _ => duplicateHelper(inCounter - 1, element, element :: tail)

    if list == List() then list
    else if duplicateNumberList.head < 0 then duplicateHelper(0, list.head, duplicate(list.tail, duplicateNumberList.tail))
    else duplicateHelper(duplicateNumberList.head, list.head, duplicate(list.tail, duplicateNumberList.tail))
  }

  // 4,5
  trait Debug {
    def debugName()  = getClass.getSimpleName
    def debugVars()  = {
      getClass.getDeclaredFields.toList.map(field =>  {
        field.setAccessible(true)
        (field.getName, field.getType, field.get(this))
      })
    }
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }


  def main(args: Array[String]) : Unit = {
    println(pickEachNElem(LazyList(5,6,3,2,1), 2, 3).toList)
    println(pickEachNElem(LazyList(5,6,3,2,1), 2, 4).toList)
    println(lazyExecute(LazyList(1,2,3), LazyList(2,3,4,5), '+').toList)
    println(duplicate(List(1,2,3), List(-1,3,1,4)))
    val point = Point(1,2)
    println(point.debugName())
    println(point.debugVars())
  }

}
