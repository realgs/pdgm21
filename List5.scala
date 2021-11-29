object List5 {

  //task1

  def conversionFromDecimal(decimalNumber: Int)(system: Int): List[Int] =
    if system < 2 then throw new Exception("Bad system")
    if decimalNumber > 0 then
      def conversionFromDecimalRec(decimalNumber: Int)(systemNumber: List[Int]): List[Int] =
        if decimalNumber > 0 then conversionFromDecimalRec(decimalNumber / system)((decimalNumber % system)::systemNumber)
        else systemNumber
      conversionFromDecimalRec(decimalNumber)(List())
    else if decimalNumber == 0 then List(0)
    else throw new Exception("Bad decimal number")


  //task2
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  val generator = scala.util.Random

  def generateNDepthTree(N: Int): BT[Float] =
    if N > 0 then Node(generator.nextFloat(),generateNDepthTree(N-1),generateNDepthTree((N-1)))
    else Empty


  //task3

  def productOfElementsFromTree(tree: BT[Float]): Float =
    tree match
      case Node(value,left,right) => value * productOfElementsFromTree(left) * productOfElementsFromTree(right)
      case Empty => 1


  //task4

  def reverse[A](xs: List[A]): List[A] =
    def reverseTail[A](xs: List[A], result: List[A]): List[A] =
      if xs == Nil then result
      else reverseTail(xs.tail, xs.head::result)
    reverseTail(xs, Nil)


  def removeDuplicatesBFS[A](tree: BT[A])(replaceValue: A): (BT[A],Set[A]) =
    def removeDuplicatesBFSRec[A](queue: List[(BT[A],Int)])(uniqueValues: Set[A])(listOfValues: List[(A,Int)])(replaceValue: A): (List[(A,Int)],Set[A]) =
      queue match
        case (Node(value,left,right), index)::tail =>
          if uniqueValues.contains(value) then removeDuplicatesBFSRec(tail:::List((left,2*index),(right,2*index +1)))(uniqueValues)((replaceValue,index)::listOfValues)(replaceValue)
          else removeDuplicatesBFSRec(tail:::List((left,2*index),(right,2*index +1)))(uniqueValues + value)((value,index)::listOfValues)(replaceValue)
        case (Empty,index)::tail => removeDuplicatesBFSRec(tail)(uniqueValues)(listOfValues)(replaceValue)
        case Nil => (listOfValues,uniqueValues)
    val (listOfValues,uniqueValues) = removeDuplicatesBFSRec(List((tree,1)))(Set())(Nil)(replaceValue)
    (makeTreeFromList(reverse(listOfValues)),uniqueValues)


  def makeTreeFromList[A](listOfNodes: List[(A,Int)]): BT[A] =
    def makeTreeFromListRec[A](listOfNodes: List[(A,Int)])(index: Int): BT[A] =
      listOfNodes match
        case (value,indexFromList)::tail =>
          if indexFromList == index then Node(value,makeTreeFromListRec(tail)(2*index),makeTreeFromListRec(tail)(2*index+1))
          else if indexFromList > index then Empty
          else makeTreeFromListRec(tail)(index)
        case Nil => Empty
    makeTreeFromListRec(listOfNodes)(1)




  def removeDuplicatesDFS[A](tree: BT[A])(replaceValue: A): (BT[A],Set[A]) =
    def removeDuplicatesDFSRec[A](tree: BT[A])(replaceValue: A)(uniqueValues: Set[A]): (BT[A],Set[A]) =
      tree match
        case Node(value, left, right) =>
          val (leftTree, newUniqueValues) = removeDuplicatesDFSRec(left)(replaceValue)(uniqueValues)
          val (rightTree, newerUniqueValues) = removeDuplicatesDFSRec(right)(replaceValue)(newUniqueValues)
          if newerUniqueValues.contains(value) then (Node(replaceValue, leftTree, rightTree), newerUniqueValues)
          else (Node(value, leftTree, rightTree), newerUniqueValues + value)
        case Empty => (Empty, uniqueValues)
    removeDuplicatesDFSRec(tree)(replaceValue)(Set())



  def main(args: Array[String]): Unit = {

    println("ConversionFromDecimal tests:")
    println(conversionFromDecimal(31)(16))
    println(conversionFromDecimal(32)(16))
    println(conversionFromDecimal(256)(16))
    //println(conversionFromDecimal(25)(1))
    //println(conversionFromDecimal(-6)(16))
    println(conversionFromDecimal(234)(7))
    println(conversionFromDecimal(88)(2))
    println(conversionFromDecimal(1111111)(10))
    println(conversionFromDecimal(8658)(16))
    println()


    println("GenerateNDepthTree tests:")
    val t1 = generateNDepthTree(4)
    println(t1)
    val t2 = generateNDepthTree(1)
    println(t2)
    val t3 = generateNDepthTree(2)
    println(t3)
    val t4 = generateNDepthTree(0)
    println(t4)
    println()


    println("ProductOfElementsFromTree tests:")
    println(productOfElementsFromTree(t1))
    println(productOfElementsFromTree(t2))
    println(productOfElementsFromTree(t3))
    println(productOfElementsFromTree(t4))
    println()



    val bt = Node(1,Node(1,Node(2,Empty,Empty),Node(3,Empty,Empty)),Node(4,Node(5,Empty,Empty),Node(5,Empty,Empty)))
    val bt1 = Node(1,Node(1,Node(1,Empty,Empty),Node(1,Empty,Empty)),Node(1,Node(1,Empty,Empty),Node(1,Empty,Empty)))
    val bt2 = Node(1,Node(1,Node(2,Node(1,Empty,Empty),Empty),Node(3,Empty,Empty)),Node(4,Node(5,Empty,Node(1,Empty,Empty)),Node(5,Empty,Empty)))

    println("RemoveDuplicatesDFS tests:")
    println(bt)
    println(removeDuplicatesDFS(bt)(0))
    println()
    println(bt1)
    println(removeDuplicatesDFS(bt1)(0))
    println()
    println(bt2)
    println(removeDuplicatesDFS(bt2)(0))
    println()
    println()

    println("RemoveDuplicatesBFS tests:")
    println(bt)
    println(removeDuplicatesBFS(bt)(0))
    println()
    println(bt1)
    println(removeDuplicatesBFS(bt1)(0))
    println()
    println(bt2)
    println(removeDuplicatesBFS(bt2)(0))
    println()
    println()
  }
}
