package Project
import java.util.Scanner
import scala.collection.mutable.Map
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, TimeoutException, duration}
import concurrent.duration.DurationInt
import scala.util.Random
import scala.concurrent.*
import ExecutionContext.Implicits.global

class Server() {
  private var b = Map[Int,Int]((1,4),(2,4),(3,4),(4,4),(5,4),(6,4),(7,0),(8,4),(9,4),(10,4),(11,4),(12,4),(13,4),(14,0))
  private var g = new Game

  def board: Map[Int,Int] = b
  def game: Game = g

  def board_(newBoard:Map[Int,Int]): Unit =
    b = newBoard

  def game_(newGame:Game): Unit =
    g = newGame

  def chooseGame(): Unit =
    println("Wybierz tryb gry: ")
    println("1) - Symulacja")
    println("2) - Gracz vs. Komputer")
    println("3) - Gracz0 vs. Gracz1")
    var sc = new Scanner(System.in)
    val int = sc.nextInt()
    int match
      case 1 => startBothSimulationGame()
      case 2 => startPersonSimulationGame()
      case 3 => startBothPeopleGame()
      case _ => println("Zły wybór")

  def startBothSimulationGame(): Unit =
    game_(new Game)
    println(board.toList)
    val rnd = new Random()
    var whosTurn = rnd.nextInt(2)
    var current = Node(-1,0,board,g.createDecisionTree(whosTurn,board),false)
    while isAbleToMakeMove(whosTurn) do
      println("Turn player number: " + whosTurn)
      current = g.findTheBestChoice(current.choicesList)
      board_(current.board)
      if !current.isNextMove then
        whosTurn = math.abs(whosTurn-1)
        current.choicesList_(g.createDecisionTree(whosTurn,board))
      println(board.toList)
    end while

  def startPersonSimulationGame(): Unit =
    game_(new Game)
    println(board.toList)
    val rnd = new Random()
    var whosTurn = rnd.nextInt(2)
    var current = Node(-1,0,board,g.createDecisionTree(whosTurn,board),false)
    val sc = new Scanner(System.in)
    var int = -1
    while isAbleToMakeMove(whosTurn) do
      try {
        val f1 = Future {
          println("Turn player number: " + whosTurn)
          if whosTurn == 0 then
            println("Choose number of your house (1-6): ")
            int = sc.nextInt()

            while !isIntValid(int, whosTurn) do
              println("Wrong choice")
              println("Choose number of your house (1-6): ")
              int = sc.nextInt()
            end while

            val move = g.makeAMove(board, int)
            board_(move._1)
            if move._2 == 7 then
              current = Node(int, board(7), board, List(), true)
            else
              current = Node(int, board(7), board, List(), false)
          else
            current = g.findTheBestChoice(current.choicesList)
            board_(current.board)

          if !current.isNextMove then
            if whosTurn == 0 then
              whosTurn = 1
            current.choicesList_(g.createDecisionTree(whosTurn, board))
            else
              whosTurn = 0
          println(board.toList)
        }
        Await.result(f1,10.seconds)
      }
      catch {
        case e:
          TimeoutException => println("run out of time")
          return ()
      }
    end while


  def startBothPeopleGame(): Unit =
    println(board.toList)
    val rnd = new Random()
    var whosTurn = rnd.nextInt(2)
    var sc = new Scanner(System.in)
    var int = -1
    while isAbleToMakeMove(whosTurn) do
      try {
        val f1 = Future {
          println("Turn player number: " + whosTurn)
          if whosTurn == 0 then
            println("Choose number of your house (1-6): ")
            int = sc.nextInt()

            while !isIntValid(int,whosTurn) do
              println("Wrong choice")
              println("Choose number of your house (1-6): ")
              int = sc.nextInt()
            end while

            val move = g.makeAMove(board,int)
            board_(move._1)
            if move._2 != 7 then whosTurn = math.abs(whosTurn - 1)
          else
            println("Choose number of your house (8-13): ")
            int = sc.nextInt()

            while !isIntValid(int,whosTurn) do
              println("Wrong choice")
              println("Choose number of your house (8-13): ")
              int = sc.nextInt()
            end while

            val move = g.makeAMove(board,int)
            board_(move._1)
            if move._2 != 14 then whosTurn = math.abs(whosTurn - 1)

          println(board.toList)
        }
        Await.result(f1,10.seconds)
      }
      catch {
        case e:
          TimeoutException => println("run out of time")
          return ()
      }
    end while


  def isIntValid(number: Int, id: Int):Boolean =
    for(i <- board)
      if i._2 == 0 && number == i._1 then return false

    if id == 0 then
      if number < 1 || number > 6 then return false
      else true
    else
      if number < 8 || number > 13 then return false
      else true

  def isAbleToMakeMove(player: Int): Boolean =
    if player == 0 then
      var i = 1
      while i<7 do
        if board(i) != 0 then return true
        else i=i+1
      end while
      return false
    else
      var i = 8
      while i<14 do
        if board(i) != 0 then return true
        else i=i+1
      end while
      return false

}
