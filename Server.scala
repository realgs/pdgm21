package server
import Board.Board
import Player.Player
import _root_.Player.PlayerHuman
import _root_.Player.AI
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.StdIn.readLine
import java.util.concurrent.{TimeUnit, TimeoutException}
import concurrent.ExecutionContext.Implicits.global



class Server {
  private var Board: Board = _
  private var Player1: Player = _
  private var Player2: Player = _
  private var CurrentPlayer: Int = _
  private var BoardSize: Int = 6
  private var StartStones: Int = 6
  private var MaxTime: Int = 30


  def start(): Unit =
    println("Choose size of board: ")
    var read: String = readLine()
    if read == "" then BoardSize
    else BoardSize = read.toInt
    println("Choose number of start stones: ")
    read = readLine()
    if read == "" then StartStones
    else StartStones = read.toInt
    Board = new Board(BoardSize, StartStones)
    println("choose style of play 1) 2 human players 2) 2 Ai players 3) human vs Ai")
    read = readLine()
    read match {
      case "1" =>
        println("Player 1 Name: ")
        read = readLine()
        Player1 = new PlayerHuman(0, read)
        println("Player 2 Name: ")
        read = readLine()
        Player2 = new PlayerHuman(1, read)
        Humangame()
      case "2" =>
        Player1 = new AI(0, "Ai1")
        Player2 = new AI(1, "Ai2")
        AiGame()
      case "3" =>
        println("Human Player Number: ")
        read = readLine()
        println("Human Player name: ")
        var name = readLine()
        if read.toInt == 0 then {
          Player1 = new PlayerHuman(0, name);
          Player2 = new AI(1, "Ai1")
        }
        else {
          Player2 = new PlayerHuman(1, name);
          Player1 = new AI(0, "Ai2")
        }
        HumanAiGame()
      case _ => println("Try again"); start()
    }

    def getActBoard(): Board =
      return Board

  def Humangame(): Unit = {
    Board.PrintBoard()
    if CurrentPlayer == 0 then {
      println(s"${Player1.getName()} is moving")
      var Res = Future {
        Board.Move(readLine().toInt, Player1.getPlayerNum())
      }
      try {
        val choise = Await.result(Res, Duration(5, TimeUnit.SECONDS))
        CurrentPlayer = choise
        if Board.IsFinished(CurrentPlayer) then println(s"${Player1.getName()} wygrał")
        else Humangame()
      }
      catch {
        case _: TimeoutException => {
          println("Time is over")
          CurrentPlayer = 1
          Humangame()
        }
      }
    }
    else {
      println(s"${Player2.getName()} is moving")
      var makemove = Future {
        Board.Move(readLine().toInt, Player2.getPlayerNum())
      }
      try {
        var Res = Await.result(makemove, Duration(5, TimeUnit.SECONDS))
        if Board.IsFinished(CurrentPlayer) then
          Board.Result() match {
            case 0 => println(s"${Player1.getName()} wygrał")
            case 1 => println(s"${Player2.getName()} wygrał")
          }
        else
          CurrentPlayer = Res
          Humangame()
      }
      catch {
        case _: TimeoutException =>
          println("Time is over")
          CurrentPlayer = 0
          Humangame()
      }
    }
  }

  def AiGame(): Unit = {
    Board.PrintBoard()
    if CurrentPlayer == 0 then {
      println(s"${Player1.getName()} is moving")
      val hole = Player1.asInstanceOf[AI].BestScore2(Board)
      val Res = Board.Move(hole, Player1.getPlayerNum())
      if Board.IsFinished(CurrentPlayer) then
        Board.Result() match {
          case 0 => println(s"${Player1.getName()} wygrał")
          case 1 => println(s"${Player2.getName()} wygrał")
        }
      else
        CurrentPlayer = Res
        AiGame()
    }
    else {
      println(s"${Player2.getName()} is moving")
      val hole = Player2.asInstanceOf[AI].BestScore(Board, CurrentPlayer)
      val Res = Board.Move(hole, Player2.getPlayerNum())
      if Board.IsFinished(CurrentPlayer) then
        Board.Result() match {
          case 0 => println(s"${Player1.getName()} wygrał")
          case 1 => println(s"${Player2.getName()} wygrał")
        }
      else
        CurrentPlayer = Res
        AiGame()
    }
  }

  def HumanAiGame(): Unit = {
    Board.PrintBoard()
    if CurrentPlayer == 0 then {
      println(s"${Player1.getName()} is moving")
      var makemove = Future {
        Board.Move(readLine().toInt, Player1.getPlayerNum())
      }
      try {
        val Res = Await.result(makemove, Duration(5, TimeUnit.SECONDS))
        if Board.IsFinished(CurrentPlayer) then
          Board.Result() match {
            case 0 => println(s"${Player1.getName()} wygrał")
            case 1 => println(s"${Player2.getName()} wygrał")
          }
        else
          CurrentPlayer = Res
          HumanAiGame()
      }
      catch {
        case _: TimeoutException =>
          println("Time is over")
          CurrentPlayer = Player2.getPlayerNum()
          HumanAiGame()
      }
    }
    else {
      println(s"${Player2.getName()} is moving")
      val hole = Player2.asInstanceOf[AI].BestScore2(Board)
      val Res = Board.Move(hole, Player2.getPlayerNum())
      if Board.IsFinished(CurrentPlayer) then
        Board.Result() match {
          case 0 => println(s"${Player1.getName()} wygrał")
          case 1 => println(s"${Player2.getName()} wygrał")
        }
      else
        CurrentPlayer = Res
        HumanAiGame()
    }
  }
}

