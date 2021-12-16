import scala.annotation.tailrec

object Lista6 {

  def eachNElement[A](list: LazyList[A], nEach: Int, mEnd: Int): LazyList[A] =
    @tailrec
    def moveHelper[A](tail: LazyList[A], leftToGo: Int, position: Int, result: LazyList[A]): LazyList[A] =
      (leftToGo, tail) match
        case(-1, _) => result.reverse                   //jeśli miniemy indeks końcowy
        case(_, LazyList()) => result.reverse           //jeśli lista się skończy
        case(distance, head#::tail) =>
          if position != nEach
            then moveHelper(tail, leftToGo-1, position + 1, result)
          else moveHelper(tail, leftToGo-1, 1, head#::result)
    moveHelper(list, mEnd, nEach, LazyList())


  def lazyExecute(first: LazyList[Int], second: LazyList[Int], operator: Char): LazyList[Int] =
    @tailrec
    def moveHelper(firstTail: LazyList[Int], secondTail: LazyList[Int], result: LazyList[Int]): LazyList[Int] =

      def compute(firstElem: Int, secondElem: Int): Int =
        operator match
          case '+' => firstElem + secondElem
          case '-' => firstElem - secondElem
          case '*' => firstElem * secondElem
          case '/' => if secondElem != 0 then firstElem / secondElem else throw Exception("can't divide by zero!")
          case _ => throw Exception("it's not a valid operator")

      (firstTail, secondTail) match
        case (LazyList(), LazyList()) => result.reverse
        case (fHead#::fTail, LazyList()) => moveHelper(LazyList(), LazyList(), firstTail.reverse#:::result)
        case (LazyList(), sHead#::sTail) => moveHelper(LazyList(), LazyList(), secondTail.reverse#:::result)
        case (fHead#::fTail, sHead#::sTail) => moveHelper(fTail, sTail, compute(fHead, sHead)#::result)
    moveHelper(first, second, LazyList())



  def duplicate[A](elementList: List[A], howManyList: List[Int]): List[A] =
    
    @tailrec
    def moveHelper(elementTail: List[A], howManyTail: List[Int], result:List[A]): List[A] =

      @tailrec
      def duplicateHelper(element: A, howMuchToGo: Int, result: List[A]): List[A] =
        howMuchToGo match
          case 0 => result
          case _ => duplicateHelper(element, howMuchToGo-1, element::result)

      (elementTail, howManyTail) match
        //case (LazyList(), LazyList()) => result.reverse
        case (List(), _) => result.reverse
        case (elHead::elTail, List()) => moveHelper(List(), List(), duplicateHelper(elHead, 0, elementTail.reverse:::result))
        case (elHead::elTail, toGoHead::toGoTail) => moveHelper(elTail, toGoTail, duplicateHelper(elHead, toGoHead, result))
        
    moveHelper(elementList, howManyList, List())



  def main(args: Array[String]): Unit = {

    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).force)
    println(lazyExecute(LazyList(1, 2, 3, 4,5),LazyList(1, 2, 3, 4,5,6),'*').force)
    println(lazyExecute(LazyList(1, 2, 3, 4,5, 6, 7, 8, 9, 10),LazyList(1, 2, 3, 4,5,6),'+').force)
    println(duplicate(List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10), List(1, 2, 3, 4)))

    var p : Point = new Point(3,4);
    p.debugName();
    p.debugVars();


  }
}

trait Debug{

  def debugName(): Unit =
    println(getClass())

  def debugVars(): Unit =
    var varList = List[Any]()

    getClass().getDeclaredFields().toList.map(item => {
      item.setAccessible(true)                  //cannot access a member of class Point with modifiers "private"
      varList = List((item.getName(), item.getType(), item.get(this)))::varList
    })
    println(varList.reverse)

}

class Point(xv: Int, yv: Int) extends Debug {
  var x: Int = xv
  var y: Int = yv
  var a: String = "test"
}

