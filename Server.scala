package Kalaha

import Kalaha.Board.Board.*
import Kalaha.Board.KalahaBoard.*
import Kalaha.Player.HumanPlayer.*
import Kalaha.Player.Player.*
import Kalaha.Player.RandomComputerPlayer.*
import Kalaha.Player.SmartComputerPlayer.*

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.DurationInt
import scala.concurrent.{Await, Future, TimeoutException}
import scala.io.StdIn.readLine

object Server {

  class Server():
    private val board: Board = new KalahaBoard()
    private var playerA: Player = _
    private var playerB: Player = _

    initialize()

    def runGame(): Unit =
      var nextMovePlayer = "A"

      while !board.endOfGame() do
        board.showBoard()

        if nextMovePlayer == "A" then
          println("Player A turn:")
          nextMovePlayer = makeMoveA()
        else
          println("Player B turn:")
          nextMovePlayer = makeMoveB()

      board.showBoard()
      board.gamesEnd()

    private def makeMoveA(): String =
      var move = -1

      try
        while move == -1 do
          move = Await.result(Future {
            playerA.chooseNextMove()
          }, 30.seconds)

          move = board.checkMoveA(move)
      catch
        case e: TimeoutException =>
          println("End of time. The first possible move will be made.")
          move = board.firstAvailableMoveA()

      println(s"Player A chose pit number ${move + 1}")

      playerA.makeMove(move)
      playerB.makeMove(move)

      if board.nextMoveA(move) then
        "A"
      else
        "B"

    private def makeMoveB(): String =
      var move = -1

      try
        while move == -1 do
          move = Await.result(Future {
            playerB.chooseNextMove()
          }, 30.seconds)

          move = board.checkMoveB(move)
      catch
        case e: TimeoutException =>
          println("End of time. The first possible move will be made.")
          move = board.firstAvailableMoveB()

      println(s"Player B chose pit number ${move + 1}")

      playerA.makeMove(move)
      playerB.makeMove(move)

      if board.nextMoveB(move) then
        "B"
      else
        "A"

    private def initialize(): Unit =
      println(
        "Welcome in KALAHA game !\n\n" +
          "Classic rules:\n" +
          "1. The board has 6 small pits, called houses, on each side; and a big pit, called an end zone or store, at each end. The object of the game is to capture more seeds than one's opponent.\n" +
          "2. At the beginning of the game, four seeds are placed in each house. This is the traditional method.\n" +
          "3. Each player controls the six houses and their seeds on the player's side of the board. The player's score is the number of seeds in the store to their right.\n" +
          "4. Players take turns sowing their seeds. On a turn, the player removes all seeds from one of the houses under their control.\n" +
          "   Moving counter-clockwise, the player drops one seed in each house in turn, including the player's own store but not their opponent's.\n" +
          "5. If the last sown seed lands in an empty house owned by the player, and the opposite house contains seeds, both the last seed and the opposite seeds are captured and placed into the player's store.\n" +
          "6. If the last sown seed lands in the player's store, the player gets an additional move. There is no limit on the number of moves a player can make in their turn.\n" +
          "7. When one player no longer has any seeds in any of their houses, the game ends. The other player moves all remaining seeds to their store, and the player with the most seeds in their store wins.\n\n" +
          "Additional rules:\n" +
          "1. Every player have 30 seconds to choose his move.\n" +
          "2. If player run out of time, first available move will be choose for him.\n" +
          "3. Player's pits are numbered from 1 to 6 starting with the first pit to the left of the player.\n" +
          "4. Player's store are placed at the end of the board to the right of the player.\n"
      )
      println("Press enter to continue")
      readLine()

      var legalInput = false
      while (!legalInput)
        println("Who is a first player (player A) ? (choose option from 1 to 3)")
        println("1 - random computer")
        println("2 - smart computer")
        println("3 - human\n")

        readLine() match
          case "1" =>
            playerA = new RandomComputerPlayer()
            legalInput = true
          case "2" =>
            playerA = new SmartComputerPlayer("A")
            legalInput = true
          case "3" =>
            playerA = new HumanPlayer()
            legalInput = true
          case _ =>
            println("This option doesn't exist !")

      legalInput = false
      while (!legalInput)
        println("Who is a second player (player B) ? (choose option from 1 to 3)")
        println("1 - random computer")
        println("2 - smart computer")
        println("3 - human\n")

        readLine() match
          case "1" =>
            playerB = new RandomComputerPlayer()
            legalInput = true
          case "2" =>
            playerB = new SmartComputerPlayer("B")
            legalInput = true
          case "3" =>
            playerB = new HumanPlayer()
            legalInput = true
          case _ =>
            println("This option doesn't exist !")
}
