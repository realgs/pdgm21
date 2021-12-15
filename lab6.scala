import scala.annotation.tailrec

object lab6 {

  //Zadanie 1
  def eachNElement[A](lazyList: LazyList[A], n: Int, m: Int): LazyList[A] =
    def innerEachNElement(lazyList: LazyList[A], index: Int): LazyList[A] =
      if index == m then LazyList()
      else if lazyList == LazyList() then LazyList()
      else if index % n == 0 then lazyList.head #:: innerEachNElement(lazyList.tail, index+1)
      else innerEachNElement(lazyList.tail, index+1)
    innerEachNElement(lazyList, 0)

  //Zadanie 2
  def lazyExecute(firstLazyList: LazyList[Int], secondLazyList: LazyList[Int], sign: Char): LazyList[Int] =
    @tailrec
    def innerLazyExecute(firstLazyList: LazyList[Int], secondLazyList: LazyList[Int], resultList: LazyList[Int]): LazyList[Int] =
      (firstLazyList, secondLazyList) match
        case (LazyList(), LazyList()) => resultList
        case (_, LazyList()) => (resultList #::: firstLazyList).reverse
        case (LazyList(), _) => (resultList #::: secondLazyList).reverse
        case (_, _) =>
          sign match
            case '+' => innerLazyExecute(firstLazyList.tail, secondLazyList.tail,
              (firstLazyList.head + secondLazyList.head) #:: resultList)
            case '-' => innerLazyExecute(firstLazyList.tail, secondLazyList.tail,
              (firstLazyList.head - secondLazyList.head) #:: resultList)
            case '*' => innerLazyExecute(firstLazyList.tail, secondLazyList.tail,
              (firstLazyList.head * secondLazyList.head) #:: resultList)
            case '/' => innerLazyExecute(firstLazyList.tail, secondLazyList.tail,
              (firstLazyList.head / secondLazyList.head) #:: resultList)
            case _ => LazyList.empty
    innerLazyExecute(firstLazyList, secondLazyList, LazyList()).reverse

  //Zadanie 3
  def duplicate[A](elements: List[A], numberOfDuplicates: List[Int]): List[A] =
    @tailrec
    def innerDuplicate[A](elements: List[A], numberOfDuplicates: List[Int], resultList: List[A]): List[A] =
      (elements, numberOfDuplicates) match
        case (h1 :: t1, h2 :: t2) => {
          if h2 == 0 then innerDuplicate(t1, t2, resultList)
          else innerDuplicate(elements, (h2-1) :: t2, h1 :: resultList)
        }
        case (_, List()) => resultList
        case (List(), _) => resultList
    innerDuplicate(elements, numberOfDuplicates, List()).reverse

  //Zadanie 4
  //Zadanie 5

  trait Debug {
    def debugName() = getClass().getSimpleName()
    def debugVars(): List[(String, String, String)] =
      getClass().getDeclaredFields().toList.map(field => {
        field.setAccessible(true)
        (field.getName().toString(), field.getType().toString(), field.get(this).toString())
      })
  }

  class Rectangle(av: Int, bv: Int) extends Debug {
    var a: Int = av;
    var b: Int = bv;
    var name: String = "Rectangle"
    var area: Int = av*bv;
  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit = {
    println(eachNElement(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), 2, 7).toList == List(1, 3, 5, 7))
    println(eachNElement(LazyList(10, 9, 8, 7, 6, 5, 4, 3, 2, 1), 2, 7).toList == List(10, 8, 6, 4))
    println(eachNElement(LazyList(), 2, 3).toList == List())
    println(eachNElement(LazyList(1), 1, 1).toList == List(1))
    println(eachNElement(LazyList(1), 2, 2).toList == List(1))
    println(eachNElement(LazyList(1, 2), 2, 2).toList == List(1))

    println()

    println(lazyExecute(LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), LazyList(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), '+').toList
      == List(2, 4, 6, 8, 10, 12, 14, 16, 18, 20))
    println(lazyExecute(LazyList(), LazyList(), '+').toList
      == List())
    println(lazyExecute(LazyList(), LazyList(), '+').toList
      == List())
    println(lazyExecute(LazyList(1, 2, 3), LazyList(), '*').toList
      == List(1, 2, 3))
    println(lazyExecute(LazyList(), LazyList(1, 2, 3), '*').toList
      == List(1, 2, 3))

    println()

    println(duplicate(List(1, 2, 3, 4, 5), List(1, 2, 3, 4, 5)) == List(1,2,2,3,3,3,4,4,4,4,5,5,5,5,5))
    println(duplicate(List(), List()) == List())
    println(duplicate(List(1), List(8)) == List(1,1,1,1,1,1,1,1))
    println(duplicate(List(1, 2), List(0, 0)) == List())

    println()

    var p : Point = new Point(3, 4);
    println(p.debugName())
    println(p.debugVars())

    println()

    var r : Rectangle = new Rectangle(3, 4);
    println(r.debugName())
    println(r.debugVars())

  }
}
