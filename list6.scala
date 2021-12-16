// Maciej Olejnik 260444

// first task

def eachNElement[T](n: Int, m: Int, lazyList: LazyList[T]): LazyList[T] =
  if n == 0 then
    throw new Exception("n cannot be zero")
  def eachNElementInner[T](i: Int, resultList: LazyList[T], lazyList: LazyList[T]): LazyList[T] =
    if lazyList == LazyList() then
      resultList
    else if i != m then
      if i % n == 0 then
        eachNElementInner(i + 1, resultList ++: LazyList(lazyList.head), lazyList.tail)
      else
        eachNElementInner(i + 1, resultList, lazyList.tail)
    else
      resultList

  eachNElementInner(0, LazyList(), lazyList)

eachNElement(2, 3, LazyList(5, 6, 3, 2, 1)).toList == List(5, 3)
eachNElement(2, 4, LazyList(5, 6, 3, 2, 1)).toList == List(5, 3)
eachNElement(5, 10, LazyList("A", "B", "C", "D", "E", "F", "G", "H", "I", "J")).toList == List("A", "F")
eachNElement(2, 4, LazyList()).toList == List()
eachNElement(100, 5, LazyList(5, 6, 3, 2, 1)).toList == List(5)
// eachNElement(0, 500, LazyList(5, 6, 3, 2, 1)).toList  /// throws an exception
eachNElement(2, 0, LazyList(5, 6, 3, 2, 1)).toList == List()

// second task

def lazyExecute(firstList: LazyList[Int], secondList: LazyList[Int], operation: Char): LazyList[Int] =
  def lazyExecuteInner(firstList: LazyList[Int], secondList: LazyList[Int], resultList: LazyList[Int]): LazyList[Int] =
    if firstList == LazyList() || secondList == LazyList() then
      if firstList != LazyList() then resultList ++: firstList
      else if secondList != LazyList() then resultList ++: secondList
      else resultList
    else
      operation match
        case '+' => lazyExecuteInner(firstList.tail, secondList.tail, resultList ++: LazyList(firstList.head + secondList.head))
        case '-' => lazyExecuteInner(firstList.tail, secondList.tail, resultList ++: LazyList(firstList.head - secondList.head))
        case '*' => lazyExecuteInner(firstList.tail, secondList.tail, resultList ++: LazyList(firstList.head * secondList.head))
        case '/' => lazyExecuteInner(firstList.tail, secondList.tail, resultList ++: LazyList(firstList.head / secondList.head))
        case _ => throw new Exception("Operation not handled")
  lazyExecuteInner(firstList, secondList, LazyList())

lazyExecute(LazyList(1, 2, 3), LazyList(0, 3, 1, 4), '+').toList == List(1, 5, 4, 4)
lazyExecute(LazyList(1, 2, 3), LazyList(0, 3, 1, 4), '-').toList == List(1, -1, 2, 4)
lazyExecute(LazyList(1, 2, 3), LazyList(0, 3, 1, 4), '*').toList == List(0, 6, 3, 4)
lazyExecute(LazyList(1, 2, 3), LazyList(1, 3, 1, 4), '/').toList == List(1, 0, 3, 4)
lazyExecute(LazyList(), LazyList(0, 3, 1, 4), '/').toList == List(0, 3, 1, 4)
lazyExecute(LazyList(), LazyList(), '/').toList == List()
// lazyExecute(LazyList(1, 2, 3), LazyList(1, 3, 1, 4), "OPERACJA").toList == List() /// throws an exception

// third task

def duplicate[T](elementsList: LazyList[T], duplicateNumbersList: LazyList[Int]): List[T] =
  def duplicateInner[T](elementsList: LazyList[T], duplicateNumbersList: LazyList[Int], resultList: List[T]): List[T] =
    def duplicateIter[T](element: T, numberOfRepetition: Int, resultList: List[T]): List[T] =
      if numberOfRepetition == 0 then
        resultList
      else
        duplicateIter(element, numberOfRepetition - 1, element :: resultList)

    if elementsList == LazyList() || duplicateNumbersList == LazyList() then
      resultList
    else
      duplicateInner(elementsList.tail, duplicateNumbersList.tail, resultList ::: duplicateIter(elementsList.head, duplicateNumbersList.head, List()))
  duplicateInner(elementsList, duplicateNumbersList, List())

duplicate(LazyList(1, 2, 3), LazyList(0, 3, 1, 4)) == List(2, 2, 2, 3)
duplicate(LazyList(), LazyList(0, 3, 1, 4)) == List()
duplicate(LazyList(1, 2, 3), LazyList()) == List()
duplicate(LazyList(1, 2), LazyList.continually(3)) == List(1, 1, 1, 2, 2, 2)
duplicate(LazyList.continually(2), LazyList(1, 2, 3, 4)) == List(2, 2, 2, 2, 2, 2, 2, 2, 2, 2)

// fourth task

trait firstDebug {
  def debugName =
    this.getClass().getName()
}

class Point(xv: Int, yv: Int) extends firstDebug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
}

var firstPoint : Point = new Point(3, 4);

firstPoint.debugName

// fifth task

trait secondDebug {
  def debugName =
    this.getClass().getName()

  def debugVars =
    def debugVarsInner(array: Array[java.lang.reflect.Field], resultList: List[AnyRef]): List[AnyRef] =   // the types are: String, Class[?], Object
      def debugVarsHelper(field: java.lang.reflect.Field): List[AnyRef] =
        field.setAccessible(true)
        List(field.getName().toString(), field.getType().toString(), field.get(this).toString())

      if array.isEmpty then
        resultList
      else
        debugVarsInner(array.tail, resultList ::: List(debugVarsHelper(array.head)))
    debugVarsInner(this.getClass().getDeclaredFields(), List())
}

class Human(name: String, surname: String, age: Int, height: Int) extends secondDebug {
    var nameInner: String = name
    var ageInner: Int = age
    var surnameInner: String = surname
    var heightInner: Int = height
}

var me : Human = new Human("Maciej", "Olejnik", 20, 183)

me.debugName
me.debugVars
