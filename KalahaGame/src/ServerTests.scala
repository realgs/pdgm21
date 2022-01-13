import GameServer.StartGame
import akka.actor.{ActorRef, ActorSystem, Props}

import scala.io.StdIn.readInt

object ServerTests extends App{

  val system = ActorSystem("ServerSystem")

  val board = new Board

  var p1choice = -1
  var p2choice = -1

  var player1: ActorRef = _
  var player2: ActorRef = _

  while(p1choice != 1 && p1choice != 2){
    println("Choose Player 1: ")
    println("1 - Human player \n2 - BotPlayer")
    try{
      p1choice = readInt()
    } catch{
      case e => println("Invalid input, must be a number")
    }
  }
  if p1choice == 1 then player1 = system.actorOf(Props(HumanPlayer(1)), "player1")
  else player1 = system.actorOf(Props(Computer(1, board)), "player1")

  while(p2choice != 1 && p2choice != 2){
    println("Choose Player 2: ")
    println("1 - Human player \n2 - BotPlayer")
    try{
      p2choice = readInt()
    } catch{
      case e => println("Invalid input, must be a number")
    }
  }
  if p2choice == 1 then player2 = system.actorOf(Props(HumanPlayer(2)), "player2")
  else player2 = system.actorOf(Props(Computer(2, board)), "player2")


  val server = system.actorOf(Props(new GameServer(player1, player2, board)))

  server ! StartGame


}
