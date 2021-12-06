package main.paradygmaty
import scala.::
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{LinkedHashMap, LinkedHashSet, ListBuffer}

object Main {

//Zadanie 1
  def eachNElement[A](list: LazyList[A], n: Int, m:Int): LazyList[A]=

    def nTailHelper(list: LazyList[A], n: Int): LazyList[A]=
      if n ==0 then list
      else if list == LazyList() then LazyList()
      else nTailHelper(list.tail, n-1)

    def eachNElementHelper(list: LazyList[A], counter: Int): LazyList[A] =
      if counter >= m then LazyList()
      else list.head #:: eachNElementHelper(nTailHelper(list, n), counter+n)

    if list.size < m then throw new IllegalArgumentException("M - finish index is bigger then list size")
    else if m<=0 then throw new IllegalArgumentException("M - finish index is less then 0")
    else if n<0 then throw new IllegalArgumentException("N - step index is not positive")
    else
      eachNElementHelper(list, 0)


  def eachNElementStream[A](list: Stream[A], n: Int, m:Int): Stream[A]=

    def nTailHelper(list: Stream[A], n: Int): Stream[A]=
      if n ==0 then list
      else if list.isEmpty then Stream()
      else nTailHelper(list.tail, n-1)

    def eachNElementHelper(list: Stream[A], counter: Int): Stream[A] =
      if counter >= m then Stream()
      else list.head #:: eachNElementHelper(nTailHelper(list, n), counter+n)

    if list.size < m then throw new IllegalArgumentException("M - finish index is bigger then list size")
    else if m<=0 then throw new IllegalArgumentException("M - finish index is less then 0")
    else if n<0 then throw new IllegalArgumentException("N - step index is not positive")
    else
      eachNElementHelper(list, 0)

//Zadanie 2
  def lExecute(list1: LazyList[Int], list2: LazyList[Int], operation: String): LazyList[Int]=
    def lExecuteHelper(list1: LazyList[Int], list2: LazyList[Int], currentOperation: (Int, Int) => Int): LazyList[Int]=
      if list1 == LazyList() then list2
      else if list2 == LazyList() then list1
      else currentOperation(list1.head, list2.head) #:: lExecuteHelper(list1.tail, list2.tail, currentOperation)

    operation match
      case "+" => lExecuteHelper(list1, list2, (x, y) => x + y)
      case "-" => lExecuteHelper(list1, list2, (x, y) => x - y)
      case "*" => lExecuteHelper(list1, list2, (x, y) => x * y)
      case "/" => lExecuteHelper(list1, list2, (x, y) => x / y)
      case _ => throw new Exception("Error: unknown operation: "+operation)

 //Zadanie 3
  def duplicateList[A](listToDuplicate: List[A], listWithDuplicationNumbers: List[Int]): List[A]=
    def duplicateHelper(listToDuplicate: List[A], listWithDuplicationNumbers: List[Int]): List[A]=
      if listToDuplicate == List() then List()
      else if listWithDuplicationNumbers.head > 0 then
        listToDuplicate.head :: duplicateHelper(listToDuplicate, listWithDuplicationNumbers.head - 1 :: listWithDuplicationNumbers.tail)
      else
        duplicateHelper(listToDuplicate.tail, listWithDuplicationNumbers.tail)

    if listWithDuplicationNumbers.size < listToDuplicate.size then throw new Exception("List with duplication numbers is too short compared to Numbers to duplicate")
    duplicateHelper(listToDuplicate, listWithDuplicationNumbers)


  def duplicateLinkedHashSet[A](setToDuplicate: LinkedHashSet[A], listDuplicateNum: List[Int]): List[A] =
    def duplicateHelper(setToDuplicate: LinkedHashSet[A], listDuplicateNum: List[Int]): List[A] =
      if setToDuplicate.isEmpty then List()
      else if listDuplicateNum.head > 0 then
        setToDuplicate.head :: duplicateHelper(setToDuplicate, listDuplicateNum.head - 1 :: listDuplicateNum.tail)
      else
        duplicateHelper(setToDuplicate.tail, listDuplicateNum.tail)
    if listDuplicateNum.size < setToDuplicate.size then throw new Exception("List with duplication numbers is too short compared to Numbers set to duplicate")
    duplicateHelper(setToDuplicate, listDuplicateNum)


  def duplicateListDuplicatesSaved[A](listToDuplicate: List[A], listDuplicateNum: List[Int]): List[A]=

    def createLinkedHashMap(list: List[A], numbers: List[Int], hashMap: LinkedHashMap[A, Int]): LinkedHashMap[A, Int]=
      if list.isEmpty || numbers.isEmpty then hashMap
      else
        if !hashMap.contains(list.head) then
          hashMap.put(list.head, numbers.head);
          createLinkedHashMap(list.tail, numbers.tail, hashMap)
        else
          hashMap.put(list.head, (hashMap.getOrElse(list.head, throw new Error("How Did We Get There?")) + numbers.head).toInt )
          createLinkedHashMap(list.tail, numbers.tail, hashMap)

    def hashMapToLists[A](hashMapList: List[(A, Int)], result: (List[A], List[Int]) ): (List[A], List[Int])=
      if hashMapList.isEmpty then result
      else hashMapToLists(hashMapList.tail, (hashMapList.head._1::result._1, hashMapList.head._2::result._2))

    def duplicateHelper(listToDuplicate: List[A], listWithDuplicationNumbers: List[Int]): List[A]=
      if listToDuplicate == List() then List()
      else if listWithDuplicationNumbers.head > 0 then
        listToDuplicate.head :: duplicateHelper(listToDuplicate, listWithDuplicationNumbers.head - 1 :: listWithDuplicationNumbers.tail)
      else
        duplicateHelper(listToDuplicate.tail, listWithDuplicationNumbers.tail)

    if listDuplicateNum.size < listToDuplicate.size then throw new Exception("List with duplication numbers is too short compared to Numbers set to duplicate")
    else
      val tupleLists = hashMapToLists( (createLinkedHashMap( listToDuplicate, listDuplicateNum, LinkedHashMap[A, Int]())).toList , (List(), List()) )
      duplicateHelper(tupleLists._1, tupleLists._2).reverse


  //Zadanie 3 i 4

  trait Debug{
    def debugName(): String =
      getClass.getName
    //Zwróci: Point

    def debugVars(): String ={
      // [[x, int, 3], [y, int, 4], [a, java.lang.String, test]]
      val listOfFields = getClass.getDeclaredFields

      val buffer = ListBuffer[String]()

      for(field <- listOfFields){
        field.setAccessible(true)
        buffer += "["+(Array(field.getName.toString, field.getType.toString, field.get(this).toString)).mkString(", ") +"]"
        field.setAccessible(false)
      }

      return "["+ (buffer.toList.toArray).mkString(", ") +"]"

    }


  }

  class Point(xv: Int, yv: Int) extends Debug {
    var x: Int = xv
    var y: Int = yv
    var a: String = "test"
  }

  def main(args: Array[String]): Unit = {

    //Testy Lista 6

    //Zadanie 1
    println()
    println("Testy zadania 1")
    println()
    val lazyTestList1 = LazyList(1,2,7,4,8,3)
    val lazyTestList2 = LazyList(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20,1323123)
    val lazyTestList3 = LazyList(5,6,3,2,1)
    println( eachNElement(lazyTestList1, 1, 4).toList == List(1,2,7,4))
    println( eachNElement(lazyTestList1, 2, 6).toList == List(1, 7, 8))
    println( eachNElement(lazyTestList2, 3, 21).toList == List(1, 4, 7, 10, 13, 16, 19))
    println( eachNElement(lazyTestList2, 5, 21).toList == List(1, 6, 11, 16, 1323123))
    println( eachNElement(lazyTestList3, 2, 3).toList == List(5,3))
    println( eachNElement(lazyTestList3, 2, 4).toList == List(5,3))

    val streamTestList1 = Stream(1,2,7,4,8,3)
    println( eachNElementStream(streamTestList1, 1, 4).toList == List(1,2,7,4))
    println( eachNElementStream(streamTestList1, 2, 6).toList == List(1, 7, 8))

    //Zadanie 2
    println()
    println("Testy zadania 2")
    println()
    val executeLazyTestList1 = LazyList(1,2,3)
    val executeLazyTestList2 = LazyList(2,3,4,5)
    println(lExecute(executeLazyTestList1, executeLazyTestList2, "+").toList == List(3,5,7,5))
    println(lExecute(executeLazyTestList1, executeLazyTestList2, "-").toList == List(-1,-1,-1,5))
    println(lExecute(executeLazyTestList1, executeLazyTestList2, "*").toList == List(2,6,12,5))
    println(lExecute(executeLazyTestList1, executeLazyTestList2, "/").toList == List(0,0,0,5))

    //Zadanie 3
    println()
    println("Testy zadania 3")
    println()

    println(duplicateList(List(1,2,3,4), List(2,3,4,5,6)) == List(1, 1, 2, 2, 2, 3, 3, 3, 3, 4, 4, 4, 4, 4))
    println(duplicateList(List(1,1,2,3,4), List(2,2,1,3,3)) == List(1, 1, 1, 1, 2, 3, 3, 3, 4, 4, 4))

    println(duplicateLinkedHashSet(LinkedHashSet(2,3,3,5,6),List(1,2,3,4,5,6)) == List(2, 3, 3, 5, 5, 5, 6, 6, 6, 6))
    println(duplicateLinkedHashSet(LinkedHashSet(1,2,3,4,5,6),List(1,2,3,4,5,6)) == List(1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6))

    println(duplicateListDuplicatesSaved(List(1,1,2,3,4), List(2,2,1,3,3)) == List(1, 1, 1, 1, 2, 3, 3, 3, 4, 4, 4))
    println(duplicateListDuplicatesSaved(List(1,2,2,2,4,4,5), List(2,2,1,3,3,1,5)) == List(1, 1, 2, 2, 2, 2, 2, 2, 4, 4, 4, 4, 5, 5, 5, 5, 5))


    //Zadanie 5 i 6
    println()
    println("Test zadania 5 i 6")
    println()

    var p : Point = new Point(3, 4);
    println(p.debugName());
    println(p.debugVars());


  }

}

