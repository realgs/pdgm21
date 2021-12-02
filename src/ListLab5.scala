import scala.::
import scala.collection.immutable.Nil.:::

object ListLab5 {
  sealed trait BT[+A]
  case object Empty extends BT[Nothing]
  case class Node[+A](elem: A, left: BT[A], right: BT[A]) extends BT[A]

  sealed trait mBT[+A]
  case object mEmpty extends mBT[Nothing]
  case class mNode[+A](elem: A, children: List[mBT[A]]) extends mBT[A]


  def main(args: Array[String]): Unit = {
    println(numberSystemChange16Final(-260))
    println(numberSystemChangeFinal(8,-2))

    val tree = createTree(3)
    println(tree)
//    println(breadthSearchBinaryDeleteTail(tree))
//    println(mulitilyTreeElements(tree))
//    println(mulitilyTreeElementsTail(tree))
//    println(depthSearchBinary(tree))
//    val tree2 = Node(1,Node(3,Empty,Node(6,Node(2,Empty,Empty),Empty)),Node(4,Node(2,Node(5,Empty,Empty),Node(2,Empty,Empty)),Empty))
//    println(breadthSearchBinaryDeleteTail(tree2))
//    println(depthSearchBinaryDelete(tree2))
    var myTree = createMyTree()
    println(depthSearchDelete(myTree))
    println(breadthSearchDelete(myTree))

    var myTree2 = createMyTree2()
    println(depthSearchDelete(myTree2))
    println(breadthSearchDelete(myTree2))

    //    println(depthSearch(myTree))
//    println(breadthSearch(myTree))
//    println(contains(myTree,11))



    //    val r = scala.util.Random
    //    println(r.nextDouble())

  }

  def power(base: Int,exponent: Int): Int =
   if exponent > 0 then base*power(base, exponent-1) else 1

  def firstLessNumber(num: Int, systemNum: Int): Int =
    if num==1 then 0
    def firstLessNumberRec(num: Int, systemNum: Int, acum: Int): Int =
      if acum > num then 0
      else if acum == num then 1
      else firstLessNumberRec(num,systemNum,acum * systemNum) + 1
    firstLessNumberRec(num, systemNum, systemNum)

  def firstLessNumberMultiply(num: Int, systemNum: Int): Int =
    if num==1 then 0
    def firstLessNumberMultiplyRec(num: Int, systemNum: Int, acum: Int): Int =
      if acum > num then 0
      else if acum == num then 1
      else firstLessNumberMultiplyRec(num,systemNum,acum + systemNum) + 1
    firstLessNumberMultiplyRec(num, systemNum, systemNum)

  def makeListFromPairOfValueAndIndex(list: List[(Int,Int)],index: Int): List[Int] =
    if list.isEmpty && index >= 0 then 0::makeListFromPairOfValueAndIndex(list, index-1)
    else if list.isEmpty  then List()
    else if list.head._2 < index then 0::makeListFromPairOfValueAndIndex(list, index-1)
    else list.head._1::makeListFromPairOfValueAndIndex(list.tail, index-1)

  def numberSystemChange16Final (number: Int): List[Int] =
    numberSystemChangeFinal(number,16)

  def numberSystemChangeFinal (number: Int,systemNum: Int): List[Int] =

    def numberSystemChange (number: Int,systemNum: Int): List[(Int,Int)] =
      val index = firstLessNumber(number,systemNum)
      val nextNum = firstLessNumberMultiply(number,power(systemNum,index))
      if number>0 then (nextNum,index)::numberSystemChange(number - nextNum*power(systemNum,index),systemNum) else List()
    if systemNum < 0 then List() else
    if number > 0 then
      val z = numberSystemChange(number,systemNum)
      0::makeListFromPairOfValueAndIndex(z,z.head._2)
    else
      val z = numberSystemChange(number.abs,systemNum)
      1::makeListFromPairOfValueAndIndex(z,z.head._2)



  def createTree(N: Int): BT[Double] =
    val r = scala.util.Random
    def createTreeRec(N: Int): BT[Double] =
      if N>0 then Node(r.nextDouble(), createTreeRec(N-1),createTreeRec(N-1))
      else Empty
    createTreeRec(N)

  def mulitilyTreeElements(tree: BT[Double]): Double =
    tree match
      case Empty => 1
      case Node(v,left,right) => v*mulitilyTreeElements(left)*mulitilyTreeElements(right)



  def createMyTree(): mBT[Double] =  mNode(1,List(mNode(2,List()),mNode(3,List(mNode(5,List(mNode(8,List()))),mNode(6,List()))),mNode(2,List(mNode(7,List()),mNode(9,List())))))

  def createMyTree2(): mBT[Double] =  mNode(1,List(mNode(2,List(mNode(4,List(mNode(8,List()))))),mNode(3,List()),mNode(4,List())))

  def contains[A](tree: mBT[A],value: A): Boolean =
    def search(stack: List[mBT[A]]): Boolean =
      stack match{
        case Nil => false
        case mNode(v,xs)::t => if v == value then true else false || search(t:::xs) }
    search (List(tree))

  def depthSearch[A](tree: mBT[A]): List[A] =
    def search(stack: List[mBT[A]]): List[A] =
      stack match{
        case Nil => List()
        case mNode(v,xs)::t => v::search(xs:::t) }

    search (List(tree))

  def breadthSearch[A](tree: mBT[A]): List[A] =
    def search(stack: List[mBT[A]]): List[A] =
      stack match{
        case Nil => List()
        case mNode(v,xs)::t => v::search(t:::xs) }
    search (List(tree))

  def takeListOfElem[A](list: List[mBT[A]]): List[A] =
    list match {
      case Nil => List()
      case mNode(v,xs)::t => v::takeListOfElem(t)
    }

  def printTree[A](list: List[mBT[A]]): Unit =
    list match {
      case List() => println("Koniec")
      case mNode(v,xs)::t =>
        println(list.head)
        printTree(xs)
    }


  def depthSearchBinary[A](tree: BT[A]): List[A] =
    def search(stack: List[BT[A]]): List[A] =
      stack match{
        case Nil => List()
        case Empty::t => search(t)
        case Node(v,l,r)::t => v::search(List(l,r):::t) }
    search (List(tree))

  def depthSearchBinaryDelete[A](tree: BT[A]):  BT[A] =
    def search(stack: List[BT[A]], containsValues: List[A]):  (BT[A],List[A]) =
      stack match{
        case Nil => (Empty,containsValues)
        case Empty::t => search(t,containsValues)
        case Node(v,l,r)::t => if containsValues.contains(v) then (Empty,containsValues) else
          val x = search(List(l),v::containsValues)
          val y = search(List(r),x._2)
          (Node(v,x._1,y._1),y._2)
      }
    search(List(tree),List())._1


  def mulitilyTreeElementsTail(tree: BT[Double]): Double =
    def mulitilyTree (queue: List[BT[Double]],acum: Double): Double =
      queue match
        case List() => acum
        case Empty::t => mulitilyTree(t,acum)
        case Node(v,l,r)::t => mulitilyTree(List(l,r):::t,acum*v  )
    mulitilyTree(List(tree),1)

  def breadthSearchBinaryDeleteTail[A](tree: BT[A]): BT[A] =
    def mulitilyTree(queue: List[BT[A]],acum: List[BT[A]], currentElems: List[A]): List[BT[A]] =
      queue match
        case List() => acum
        case Empty::t => mulitilyTree(t,acum,currentElems)
        case Node(v,l,r)::t => if currentElems.contains(v) then mulitilyTree(t,acum,currentElems) else mulitilyTree(t:::List(l,r),Node(v,l,r)::acum,v::currentElems)
    def createNewTree(tree: BT[A], nodeList: List[BT[A]]): BT[A] =
      tree match {
        case Empty => Empty
        case Node(v,l,r) => if nodeList.contains(tree) then Node(v,createNewTree(l,nodeList),createNewTree(r,nodeList)) else Empty
      }
    createNewTree(tree,mulitilyTree(List(tree),List(),List()))


  def depthSearchDelete[A](tree: mBT[A]):  mBT[A] =
    def helper(stack: List[mBT[A]],list: List[mBT[A]], containsValues: List[A]):  (List[mBT[A]],List[A]) =
      if(!list.isEmpty) then
        val x = search(List(list.head),containsValues)
        val y = helper(stack,list.tail,x._2)
        (x._1:::y._1,y._2)
      else (List(),containsValues)
    def search(stack: List[mBT[A]], containsValues: List[A]):  ( List[mBT[A]],List[A]) =
      stack match{
        case Nil => (List(),containsValues)
        case mNode(v,l)::t => if containsValues.contains(v)
        then
          val x = helper(stack,l,containsValues)
          (x._1,x._2)
        else
          val x = helper(stack,l,v::containsValues)
          (List(mNode(v,x._1)),x._2)
        case mEmpty::t =>
          search(t,containsValues)
      }
    search(List(tree),List())._1.head


  def breadthSearchDelete[A](tree: mBT[A]): mBT[A] =
    def mulitilyTree(queue: List[mBT[A]],acum: List[mBT[A]], currentElems: List[A]): List[mBT[A]] =
      queue match
        case List() => acum
        case mNode(v,l)::t => if currentElems.contains(v) then mulitilyTree(t:::l,acum,currentElems) else mulitilyTree(t:::l,mNode(v,l)::acum,v::currentElems)
        case mEmpty::t => mulitilyTree(t,acum,currentElems)
    def helper( nodeList: List[mBT[A]], childrenList: List[mBT[A]]): List[mBT[A]] =
      if !childrenList.isEmpty then createNewTree(childrenList.head,nodeList):::helper(nodeList,childrenList.tail) else List()
    def createNewTree(tree: mBT[A], nodeList: List[mBT[A]]): List[mBT[A]] =
      tree match {
        case mNode(v,l) => if nodeList.contains(tree) then List(mNode(v,helper(nodeList,l))) else helper(nodeList,l)
        case mEmpty => List()
      }
    createNewTree(tree,mulitilyTree(List(tree),List(),List())).head



//  def takeListOfElem(list: List[mBT]): List[Any] =
//    list match {
//      case Nil => List()
//      case mNode(v,xs)::t => v::takeListOfElem(t)
//    }
//

//  def depthSearchDelete[A](tree: mBT[A]): mBT[A] =
//    def search(queue: List[List[mBT[A]]], newTree: List[A]): (List[mBT[A]],List[A]) =
//      def xd(queue: List[List[mBT[A]]], childrenList: List[mBT[A]]): (List[mBT[A]], List[A]) =
//        childrenList match{
//          case Nil => (List(),List())
//          case h::t =>
//            val y = search(queue, newTree)
//            val x = xd(queue,t)
//            (x._1:::y._1,y._2:::x._2:::newTree)
//        }
//      queue match
//        case Nil => (List(),List())
//        case mNode(v,xs)::t => if newTree.contains(v) then xd( xs::queue,takeListOfElem[A](xs):::newTree)  else
//          val x = xd()
//          (List(mNode(v,x._1)),x._2)
//    search(List(tree),List())
//
//



//  def depthSearchDelete[A](tree: mBT[A]): mBT[A] =
//    def search(queue: List[List[mBT[A]]], newTree: List[A]): (List[mBT[A]],List[A]) =
//      def xd(queue: List[List[mBT[A]]], childrenList: List[mBT[A]]): (List[mBT[A]], List[A]) =
//        childrenList match{
//          case Nil => (List(),List())
//          case h::t =>
//            val y = search(queue, newTree)
//            val x = xd(queue,t)
//            (x._1:::y._1,y._2:::x._2:::newTree)
//        }
//      queue match
//        case Nil => (List(),List())
//        case mNode(v,xs)::t => if newTree.contains(v) then xd( xs::queue,takeListOfElem[A](xs):::newTree)  else
//          val x = xd()
//          (List(mNode(v,x._1)),x._2)
//    search(List(tree),List())





//DZIALA !!!!! JAK COS TO SIEGAJ
//  def depthSearchDelete[A](tree: mBT[A]):  mBT[A] =
//    def helper(stack: List[mBT[A]],list: List[mBT[A]], containsValues: List[A]):  (List[mBT[A]],List[A]) =
//      if(!list.isEmpty) then
//        val x = search(List(list.head),containsValues)
//        val y = helper(stack,list.tail,x._2)
//        (List(x._1):::y._1,y._2)
//      else (List(mEmpty),containsValues)
//
//    def search(stack: List[mBT[A]], containsValues: List[A]):  (mBT[A],List[A]) =
//    // println(stack)
//      stack match{
//        case Nil => (mEmpty,containsValues)
//        case mNode(v,l)::t => if containsValues.contains(v) then (mEmpty,containsValues) else
//          val x = helper(stack,l,v::containsValues)
//          (mNode(v,x._1),x._2)
//        case mEmpty::t =>
//          search(t,containsValues)
//      }
//    search(List(tree),List())._1


}

