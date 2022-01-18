package server
import akka.actor.{ActorRef, ActorSystem, Props}
import java.awt.event.ActionEvent
import player.{HumanPlayer, ComputerPlayer}
import gameUtensils.KalahaBoard
import server.Server.{GameOn, PlayersMove}
import scala.io.StdIn.readInt

object ServerCommunication extends App
{
  var kalahaBoard = new KalahaBoard(1)

  val system = ActorSystem("Kalaha")

  var player1: ActorRef = _
  var player2: ActorRef = _
  var server: ActorRef = _

  selectGameType()

  def selectGameType(): Unit =
    printStartMenu()
    val menuOption: Int= getMenuOption()

    menuOption match
      case 1 =>
        player1 = system.actorOf((Props(HumanPlayer(1, kalahaBoard))))
        player2 = system.actorOf((Props(HumanPlayer(2, kalahaBoard))))
        server = system.actorOf((Props(Server(kalahaBoard,  player1,  player2))))
        startGame()
      case 2 =>
        player1 = system.actorOf((Props(HumanPlayer(1, kalahaBoard))))
        player2 = system.actorOf((Props(ComputerPlayer(2, kalahaBoard))))
        server = system.actorOf((Props(Server(kalahaBoard,  player1,  player2))))
        startGame()
      case 3 =>
        player1 = system.actorOf((Props(ComputerPlayer(1, kalahaBoard))))
        player2 = system.actorOf((Props(HumanPlayer(2, kalahaBoard))))
        server = system.actorOf((Props(Server(kalahaBoard,  player1,  player2))))
        startGame()
      case 4 =>
        player1 = system.actorOf((Props(ComputerPlayer(1, kalahaBoard))))
        player2 = system.actorOf((Props(ComputerPlayer(2, kalahaBoard))))
        server = system.actorOf((Props(Server(kalahaBoard,  player1,  player2))))
        startGame()
      case 5 =>
        println("\nGoodbye!")
        System.exit(0)
      case _ =>
        println("It's not correct menu option!")
        selectGameType()


  def startGame(): Unit =
    server ! GameOn

  def printStartMenu(): Unit =
    println("\n\nKALAHA")
    println("May the best human (or not) win!\n")
    println("Choose the type of game: Player1 vs Player2")
    println("1 -> Human vs Human")
    println("2 -> Human vs Computer")
    println("3 -> Computer vs Human")
    println("4 -> Computer vs Computer")
    println("5 -> Exit")

  def getMenuOption(): Int =
    try {
        readInt()
      }
    catch {
        case e: Exception  => println("Valid menu input must be an Integer!")
          getMenuOption()
      }

}
