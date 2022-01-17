package Project
import concurrent.ExecutionContext.Implicits.global
import java.util.Scanner
import scala.collection.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.Duration
import scala.util.Random

class Node(houseNumber: Int, points: Int, b: Map[Int,Int], var choices: List[Node], nextMove: Boolean):
  def numberOfHouse = houseNumber
  def numberOfPoints = points
  def board = b
  def choicesList = choices
  def isNextMove = nextMove

  def choicesList_(newChoices: List[Node]): Unit =
    choices = newChoices

class Game :

  //       13 12 11 10 9  8
  //   14  |  |  |  |  |  |  7
  //       1  2  3  4  5  6

//  private var b = Map[Int,Int]((1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4),(14,0))
//  private var c = Node(-1,0,board,List(),false)
//
//  def board: Map[Int,Int] = b
//  def current: Node = c
//
//  def board_(newBoard:Map[Int,Int]): Unit =
//    b = newBoard
//
//  def current_(newRoot:Node): Unit =
//    c = newRoot


  //*********//

  def makeAMove(board: Map[Int,Int], numberOfHouse: Int): (Map[Int,Int],Int) =
    var seeds = board(numberOfHouse)
    var player = -1

    if numberOfHouse < 7 then
      player = 0
    else
      player = 1

    board(numberOfHouse) = 0

    if seeds == 0 then (Map((-1,-1)),-1) else
      var index = numberOfHouse + 1

      while seeds > 0 do
        board(index) = board(index) + 1
        seeds = seeds - 1
        if seeds > 0 then
          if index == 14 then index = 1
          else
            if (player == 0 && index == 13) then index = 1
            else
              if (player == 1 && index == 7) then index = index + 2
              else index = index + 1
      end while

      if index != 7 && index != 14 then
        takeSeedFromOponent(board,index)

      (board,index)



  def opositeHouse(houseNumber:Int):Int =
    var firstPart = List(1,2,3,4,5,6)
    var secondPart = List(13,12,11,10,9,8)
    if firstPart.contains(houseNumber) then secondPart.apply(houseNumber - 1)
    else firstPart.apply(math.abs(houseNumber - 13))


  def takeSeedFromOponent(board: Map[Int,Int],houseNumber:Int): Map[Int,Int] =
    if board(houseNumber) == 1 then

      val oppositeHouse = opositeHouse(houseNumber)

      if houseNumber < 7 then
        val int = 1 + board(oppositeHouse) + board(7)
        board(7) = int
        board(houseNumber) = 0
        board(oppositeHouse) = 0
        board
      else
        val int = 1 + board(oppositeHouse) + board(14)
        board(14) = int
        board(houseNumber) = 0
        board(oppositeHouse) = 0
        board
    else
      board


  //*******//


  def copyBoard(board: Map[Int,Int]):Map[Int,Int] =
    var newBoard = board.clone()
    newBoard


  def treeFirst(board: Map[Int,Int]) : List[Node] =

    val f1 = Future{
      val newBoard1 = makeAMove(copyBoard(board),1)
      newBoard1 match
        case (_,-1) => Node(1,-1,Map((-1,-1)),Nil,false)
        case (b,7) => Node(1,b(7),b,treeFirst(b),true)
        case (b,_) => Node(1,b(7),b,List(),false)
    }

    val f2 = Future{
      val newBoard2 = makeAMove(copyBoard(board),2)
      newBoard2 match
        case (_,-1) => Node(2,-1,Map((-1,-1)),Nil,false)
        case (b,7) => Node(2,b(7),b,treeFirst(b),true)
        case (b,_) => Node(2,b(7),b,List(),false)
    }

    val f3 = Future{
      val newBoard3 = makeAMove(copyBoard(board),3)
      newBoard3 match
        case (_,-1) => Node(3,-1,Map((-1,-1)),Nil,false)
        case (b,7) => Node(3,b(7),b,treeFirst(b),true)
        case (b,_) => Node(3,b(7),b,List(),false)
    }

    val f4 = Future{
      val newBoard4 = makeAMove(copyBoard(board),4)
      newBoard4 match
        case (_,-1) => Node(4,-1,Map((-1,-1)),Nil,false)
        case (b,7) => Node(4,b(7),b,treeFirst(b),true)
        case (b,_) => Node(4,b(7),b,List(),false)
    }

    val f5 = Future{
      val newBoard5 = makeAMove(copyBoard(board),5)
      newBoard5 match
        case (_,-1) => Node(5,-1,Map((-1,-1)),Nil,false)
        case (b,7) => Node(5,b(7),b,treeFirst(b),true)
        case (b,_) => Node(5,b(7),b,List(),false)
    }

    val f6 = Future{
      val newBoard6 = makeAMove(copyBoard(board),6)
      newBoard6 match
        case (_,-1) => Node(6,-1,Map((-1,-1)),Nil,false)
        case (b,7) => Node(6,b(7),b,treeFirst(b),true)
        case (b,_) => Node(6,b(7),b,List(),false)
    }
    List[Node](Await.result(f1,Duration.Inf),
      Await.result(f2,Duration.Inf),
      Await.result(f3,Duration.Inf),
      Await.result(f4,Duration.Inf),
      Await.result(f5,Duration.Inf),
      Await.result(f6,Duration.Inf))

  def treeSecond(board: Map[Int,Int]) : List[Node] =
    val f1 = Future {
      val newBoard8 = makeAMove(copyBoard(board), 8)
      newBoard8 match
        case (_,-1) => Node(8,-1,Map((-1,-1)),Nil,false)
        case (b,14) => Node(8,b(14),b,treeSecond(b),true)
        case (b,_) => Node(8,b(14),b,List(),false)
    }

    val f2 = Future {
      val newBoard2 = makeAMove(copyBoard(board), 9)
      newBoard2 match
        case (_,-1) => Node(9,-1,Map((-1,-1)),Nil,false)
        case (b,14) => Node(9,b(14),b,treeSecond(b),true)
        case (b,_) => Node(9,b(14),b,List(),false)
    }
    val f3 = Future {
      val newBoard3 = makeAMove(copyBoard(board), 10)
      newBoard3 match
        case (_,-1) => Node(10,-1,Map((-1,-1)),Nil,false)
        case (b,14) => Node(10,b(14),b,treeSecond(b),true)
        case (b,_) => Node(10,b(14),b,List(),false)
    }
    val f4 = Future {
      val newBoard4 = makeAMove(copyBoard(board), 11)
      newBoard4 match
        case (_,-1) => Node(11,-1,Map((-1,-1)),Nil,false)
        case (b,14) => Node(11,b(14),b,treeSecond(b),true)
        case (b,_) => Node(11,b(14),b,List(),false)
    }
    val f5 = Future {
      val newBoard5 = makeAMove(copyBoard(board), 12)
      newBoard5 match
        case (_,-1) => Node(12,-1,Map((-1,-1)),Nil,false)
        case (b,14) => Node(12,b(14),b,treeSecond(b),true)
        case (b,_) => Node(12,b(14),b,List(),false)
    }
    val f6 = Future {
      val newBoard6 = makeAMove(copyBoard(board), 13)
      newBoard6 match
        case (_,-1) => Node(13,-1,Map((-1,-1)),Nil,false)
        case (b,14) => Node(13,b(14),b,treeSecond(b),true)
        case (b,_) => Node(13,b(14),b,List(),false)
    }

    List[Node](Await.result(f1, Duration.Inf),
      Await.result(f2, Duration.Inf),
      Await.result(f3, Duration.Inf),
      Await.result(f4, Duration.Inf),
      Await.result(f5, Duration.Inf),
      Await.result(f6, Duration.Inf))

  def findTheBestChoice(list: List[Node]):Node =
    var theMostPoints = 0
    var theBestList = ListBuffer[Node]()
    var i = 0

    while i < list.length do
      if list.apply(i).isNextMove then
        if list.apply(i).numberOfPoints + findTheBestChoice(list.apply(i).choicesList).numberOfPoints > theMostPoints then theMostPoints = list.apply(i).numberOfPoints + findTheBestChoice(list.apply(i).choicesList).numberOfPoints
      else
        if list.apply(i).numberOfPoints > theMostPoints then theMostPoints = list.apply(i).numberOfPoints
      i = i+1
    end while

    i = 0
    while i < list.length do
      if list.apply(i).isNextMove then
        if list.apply(i).numberOfPoints + findTheBestChoice(list.apply(i).choicesList).numberOfPoints == theMostPoints then theBestList+=(list.apply(i))
      else
        if list.apply(i).numberOfPoints == theMostPoints then theBestList+=(list.apply(i))
      i = i+1
    end while

    if theBestList.isEmpty then Node(0, 0, Map((-1,-1)), Nil, false)
    else
      val theBest = theBestList.apply(Random.nextInt(theBestList.length))
      theBest


  def createDecisionTree(player: Int, board: Map[Int,Int]):List[Node] =
    if player == 0 then
      treeFirst(board)
    else
      treeSecond(board)

//  def startBothSimulationGame =
//    var board = List((1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4),(14,0))
//    println(board)
//    var root = Node(-1,0,board,treeFirst(board),false)
//    var current = root
//    var whosTurn = 0  // 0 or 1
//    while isAbleToMakeMove(board,whosTurn) do
//      println("Turn player number: " + whosTurn)
//      current = findTheBestChoice(current.choicesList)
//      board = current.board
//      if current.isNextMove then
//        whosTurn = whosTurn
//      else
//        whosTurn = math.abs(whosTurn-1)
//        current.choicesList_(createDecisionTree(whosTurn,board))
//      println(board)
//    end while
//
//
//  def startBothPeopleGame =
//    var board = List((1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4),(14,0))
//    println(board)
//    var whosTurn = 0  // 0 or 1
//    var sc = new Scanner(System.in)
//    var int = -1
//    while isAbleToMakeMove(board,whosTurn) do
//      println("Turn player number: " + whosTurn)
//      int = sc.nextInt()
//      val move = makeAMove(copyBoard(board),int)
//      board = move._1
//      if (whosTurn == 0 && move._2 == 6) || (whosTurn == 1 && move._2 == 13) then whosTurn = whosTurn else whosTurn = math.abs(whosTurn - 1)
//      println(board)
//    end while
//
//  def isIntValid(number: Int, board:List[(Int,Int)], id: Int):Boolean =
//    for(i <- board)
//      if i._2 == 0 && number == i._1 then return false
//
//    if id == 0 then
//      if number < 1 || number > 6 then return false
//      else true
//    else
//      if number < 8 || number > 13 then return false
//      else true
//
//
//  def startPersonSimulationGame =
//    var board = List((1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4),(14,0))
//    println(board)
//    var whosTurn = 0  // 0 or 1
//    var root = Node(-1,0,board,treeFirst(board),false)
//    var current = root
//    var sc = new Scanner(System.in)
//    var int = -1
//    while isAbleToMakeMove(board,whosTurn) do
//      println("Turn player number: " + whosTurn)
//      if whosTurn == 0 then
//        println("Choose number of your house (1-6): ")
//        int = sc.nextInt()
//
//        while !isIntValid(int,board,whosTurn) do
//          println("Wrong choice")
//          println("Choose number of your house (1-6): ")
//          int = sc.nextInt()
//        end while
//
//        val move = makeAMove(copyBoard(board),int)
//        board = move._1
//        if move._2 == 6 then
//          current = Node(int, board.apply(6)._2, board, List(), true)
//        else
//          current = Node(int, board.apply(6)._2, board, List(), false)
//      else
//        current = findTheBestChoice(current.choicesList)
//        board = current.board
//
//      if current.isNextMove then
//        whosTurn = whosTurn
//      else
//        if whosTurn == 0 then
//          whosTurn = 1
//          current.choicesList_(createDecisionTree(whosTurn,board))
//        else
//          whosTurn = 0
//      println(board)
//    end while

