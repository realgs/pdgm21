import GameState.copy
import Server.gametype

import scala.io.StdIn.readInt

class Server private (private val gameState: GameState, private val players: Tuple2[Player, Player]) {
  def gameStat: GameState = gameState

  def printGame: Unit =
    gameState.printRoud

  def runGame: Unit = {
    while(!gameState.isGameOver) {
      println(s"\n playr to move: ${gameState.sideToMove + 1}")
      printGame
      if gameState.sideToMove == 0 then
        gameState.updatePos(players._1.getMove(copy(gameState)))
      else
        gameState.updatePos(players._2.getMove(copy(gameState)))
    }
    if gameState.winner == -1 then
      println("draw!!!")
    else
      println(s"player: ${gameState.winner + 1} won!!!")

    println(s"score: ${gameState.score(0)} - ${gameState.score(1)}")
  }
}

object Server {
  def apply: Server = {
    println("enter number of houses and seeds in each house: ")
    val houses = readInt()
    val seeds = readInt()
    val game =
      gametype match
        case 1 => (new Engine(0, 6), new Engine(1, 6))
        case 2 => (new User(0), new User(1))
        case 3 => (new User(0), new Engine(1, 6))
        case _ => (new Engine(0, 6), new User(1))

    new Server(GameState(houses, seeds), game)
  }

  private def gametype: Int = {
    println("choose gametype: ")
    println("1. Engine vs Engine")
    println("2. Player vs Player")
    println("3. Player vs Engine")
    println("4. Engine vs Player")

    var input = readInt()
    while(input <= 0 || input > 4){
      println("please enter number between 1 and 4")
      input = readInt()
    }

    input
  }
}
