object Lista6 {

//  Zadanie 1
  def eachNElement[A](originList:LazyList[A], n:Int, m:Int) =
    def excecuteEachNElement(originList:LazyList[A], index:Int):LazyList[A] =
      if n == 0 || index == m || originList == LazyList() then LazyList()
      else if index % n == 0 then originList.head #:: excecuteEachNElement(originList.tail, index + 1)
      else excecuteEachNElement(originList.tail, index + 1)

    excecuteEachNElement(originList, 0)

// Zadanie 2
  def operate(firstValue:Int, secondValue:Int, operation:String):Int =
    if operation == "+" then firstValue + secondValue
    else if operation == "-" then firstValue - secondValue
    else if operation == "*" then firstValue * secondValue
    else firstValue / secondValue

  def lazyExecute(firstList:LazyList[Int], secondList:LazyList[Int], operation:String) =
    def lazyExecuteItter(firstList:LazyList[Int], secondList:LazyList[Int]):LazyList[Int] =
      (firstList, secondList) match
        case (LazyList(), LazyList()) => LazyList()
        case (head1 #::tail1, head2 #::tail2) => operate(head1, head2, operation) #:: lazyExecuteItter(tail1, tail2)
        case (_, LazyList()) => firstList.head #:: lazyExecuteItter(firstList.tail, LazyList())
        case (LazyList(), _) => operate(if operation != "/" && operation != "*" then 0 else 1, secondList.head, operation) #:: lazyExecuteItter(LazyList(), secondList.tail)

    lazyExecuteItter(firstList, secondList)

// Zadanie 3
  def duplicateList[A](originList:LazyList[A], duplicateCollection:LazyList[Int]) =
    def duplicate(originList:LazyList[A], duplicateCollection:LazyList[Int], howMuchToDuplicate:Int, result:LazyList[A]):LazyList[A] =
      (originList, duplicateCollection) match
        case (LazyList(), _) | (_, LazyList()) => result.reverse
        case _ =>
          if howMuchToDuplicate == 0 then duplicate(originList.tail, duplicateCollection.tail, if duplicateCollection.tail != LazyList() then duplicateCollection.tail.head else 0, result)
          else duplicate(originList, duplicateCollection, howMuchToDuplicate - 1, originList.head #:: result )

    duplicate(originList, duplicateCollection, if duplicateCollection != LazyList() then duplicateCollection.head else 0, LazyList())

//  Zadanie 4 i 5
  trait Debug:
    def debugName() = this.getClass().getSimpleName()
    def debugVars() =
      var fields = List[List[String]]()

      for(field <- this.getClass.getDeclaredFields())
        field.setAccessible(true)
        val infoListOfField = List(field.getName().toString(), field.getAnnotatedType().toString(), field.get(this).toString())
        fields = infoListOfField :: fields

      fields.reverse


  class Point(xv: Int, yv: Int) extends Debug:
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"


  def main(args: Array[String]): Unit = {
    println("eachNElement")
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 3).force)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 2, 4).force)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 0, 4).force)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 1, 5).force)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 1, 6).force)
    println(eachNElement(LazyList(5, 6, 3, 2, 1), 1, 0).force)
    println(eachNElement(LazyList(), 2, 4).force)
    println()

    println("lazyExecute")
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), "+").force)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), "-").force)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), "*").force)
    println(lazyExecute(LazyList(1, 2, 3), LazyList(2, 3, 4, 5), "/").force)
    println(lazyExecute(LazyList(1, 2, 3, 4), LazyList(5, 2), "-").force)
    println(lazyExecute(LazyList(1, 2, 3, 4), LazyList(5, 2), "/").force)
    println(lazyExecute(LazyList(), LazyList(5, 2, 3), "-").force)
    println(lazyExecute(LazyList(), LazyList(5, 2, 3), "/").force)
    println(lazyExecute(LazyList(), LazyList(5, 2, 3), "*").force)
    println(lazyExecute(LazyList(), LazyList(), "/").force)
    println()

    println("duplicateList")
    val list0 = LazyList(1, 2 ,3)
    val duplicateList0  = LazyList(0, 3, 1, 4)
    println(duplicateList(list0, duplicateList0).force)

    val list1 = LazyList(1, 2, 3, 4, 5)
    lazy val duplicateList1 : LazyList[Int] = 1 #:: 3 #:: 1 #:: 4 #:: duplicateList1
    println(duplicateList(list1, duplicateList1).force)

    val list2 = LazyList(1, 2, 3, 4, 5)
    val duplicateList2  = LazyList(1, 3, 1)
    println(duplicateList(list2, duplicateList2).force)

    val list3 = LazyList(1, 2, 3, 4, 5)
    val duplicateList3  = LazyList()
    println(duplicateList(list3, duplicateList3).force)

    val list4 = LazyList()
    val duplicateList4  = LazyList()
    println(duplicateList(list4, duplicateList4).force)
    println()

    println("debugName and debugVars")
    var p : Point = new Point(3, 4)
    println(p.debugName())
    println(p.debugVars())
  }

}
