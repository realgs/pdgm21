package game

import akka.actor.{ActorSystem, Props}
import GameServer.StartGame

import scala.io.StdIn.{readInt, readLine}

object KalahaGame extends App {

  val system = ActorSystem("ServerSystem")
  var p1choice = -1
  var p2choice = -1

  println("Welcome to Kalaha Game! \n")

  println("Instructions:\n\n" +
    "The Kalaha board has 6 small pits, called houses, on each side and a big pit, called store, at each end. \n" +
    "The object of the game is to capture more seeds than your opponent.\n" +
    "Start of the game:" +
    "1. At the beginning of the game, six seeds are placed in each house.\n" +
    "2. Each player controls the six houses and their seeds on the player's side of the board.\n" +
    "3. The player's score is the number of seeds in the store to their right.\n" +
    "4. Players take turns sowing their seeds. On a turn, the player removes all seeds from one of the houses under their control.\n" +
    "Moving counter-clockwise, the player drops one seed in each house in turn, including the player's own store but not their opponent's.\n" +
    "To choose their moves players have maximum of 30 seconds, after which the game will be terminated\n" +
    "5. If the last sown seed lands in an empty house owned by the player, and the opposite house contains seeds,\n" +
    "both the last seed and the opposite seeds are captured and placed into the player's store.\n" +
    "6. If the last sown seed lands in the player's store, the player gets an additional move.\n" +
    "End of game:\n" +
    "If one of the players runs out of the time, the game finishes with WALKOVER.\n" +
    "When one player no longer has any seeds in any of their houses, the game ends.\n" +
    "The other player moves all remaining seeds to their store, and the player with the most seeds in their store wins.\n")

  println("Are you ready? Press ENTER\n")
  readLine()

  println("Choose your players' modes: \n")

  while (p1choice != 1 && p1choice != 2) {
    println("Choose Player 1: ")
    println("1 - Human player   2 - Bot player")
    try {
      p1choice = readInt()
    } catch {
      case e => println("Invalid input, must be a number")
    }
  }
  while (p2choice != 1 && p2choice != 2) {
    println("Choose Player 2: ")
    println("1 - Human player   2 - Bot player")
    try {
      p2choice = readInt()
    } catch {
      case e => println("Invalid input, must be a number")
    }
  }

  val server = system.actorOf(Props(new GameServer(p1choice, p2choice)))

  println("The game is beginning!\n")

  server ! StartGame
}
