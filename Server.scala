package Server

import Game.Board
import Client.Client
import _root_.Client.Computer
import _root_.Client.Player

import java.util.concurrent
import scala.concurrent.Promise
import scala.io.StdIn.*
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.{DurationInt, FiniteDuration}
import java.util.concurrent.{TimeUnit, TimeoutException}
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success, Try}
import scala.concurrent.*
import scala.concurrent.ExecutionContext.Implicits.global

class Server {
  private var board: Board = _
  private var player1: Client = _
  private var player2: Client = _
  private val PLAYER1 = 1
  private val PLAYER2 = 2


  def chooseGame: Int =
    println("Wybierz grę:")
    println("1.Gracz vs gracz")
    println("2.Gracz vs komputer")
    println("3.Komputer vs komputer")
    readInt

  def initializeGame: Unit =
    val choose: Int = chooseGame
    board = new Board
    choose match
      case 1 => {
        println("Gracz1 -> Podaj nick: ")
        player1 = new Player(readLine(), PLAYER1)
        println("Gracz2 -> Podaj nick: ")
        player2 = new Player(readLine(), PLAYER2)
      }
      case 2 => {
        println("Gracz1 -> Podaj nick: ")
        player1 = new Player(readLine(), PLAYER1)
        player2 = new Computer("Computer", PLAYER2)
      }
      case 3 => {
        player1 = new Computer("Computer1", PLAYER1)
        player2 = new Computer("Computer2", PLAYER2)
      }
      case _ => {
        println("Nieprawidłowy numer")
        choice
      }

  def choice: Unit =
    println("Co chcesz zrobić?")
    println("1.Zakończ grę")
    println("2.Zagraj jeszcze raz")
    readInt match
      case 1 => println("Dziekuję za grę")
      case 2 => startGame

  def startGame: Unit =
    initializeGame
    var player: Int = PLAYER1
    var continue = true
    implicit val ec = ExecutionContext.global
    val maxWaitTime: FiniteDuration = Duration(30, TimeUnit.SECONDS)
    while (!board.isGameFinished && continue) {
      if player == PLAYER1 then println(board.showBoard1)
      else println(board.showBoard2)
      println("Gracz nr: " + player)
      println("Podaj numer dołka: ")
      if player == PLAYER1 then {
        try {
          val result = Future(board.movePebbles(player1.whichHole(board), PLAYER1))
          player = Await.result(result, maxWaitTime)
        }
        catch {
          case _: TimeoutException => {
            println("Czas minął")
            println("Gra została zakończona")
            continue = false
          }
        }
      }
      else {
        try {
          val result = Future(board.movePebbles(player2.whichHole(board), PLAYER2))
          player = Await.result(result, maxWaitTime)
        }
        catch {
          case _: TimeoutException => {
            println("Czas minął")
            println("Gra została zakończona")
            continue = false
          }
        }
      }
    }
      if (player == PLAYER1) then println(board.showBoard2)
      else println(board.showBoard1)
      val result = board.results
      val name = if (result == 1) then player1.getName
                 else if(result == 2) player2.getName
                 else "remis"
      println("Wygrał gracz : " + name)
      println("Dziękuję za grę")
}
