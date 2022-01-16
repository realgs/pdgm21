package Kalaha.Board

import Kalaha.Board.Board.*

object KalahaBoard {

  class KalahaBoard(newPitsA: Array[Int] = Array(4, 4, 4, 4, 4, 4, 0), newPitsB: Array[Int] = Array(4, 4, 4, 4, 4, 4, 0)) extends Board() :
    val pitsA: Array[Int] = newPitsA.clone()
    val pitsB: Array[Int] = newPitsB.clone()

    override def copy(): KalahaBoard =
      new KalahaBoard(pitsA, pitsB)

    override def endOfGame(): Boolean =
      var noMoreMovesA = true
      for i <- 0 to 5 do
        if pitsA(i) != 0 then noMoreMovesA = false

      var noMoreMovesB = true
      for i <- 0 to 5 do
        if pitsB(i) != 0 then noMoreMovesB = false

      noMoreMovesA || noMoreMovesB

    override def gamesEnd(): Unit =
      println("************* End of the game *************")

      for i <- 0 to 5 do
        pitsA(6) += pitsA(i)
        pitsA(i) = 0

      for i <- 0 to 5 do
        pitsB(6) += pitsB(i)
        pitsB(i) = 0

      showBoard()

      println(s"Player A score: ${pitsA(6)}")
      println(s"Player B score: ${pitsB(6)}")

      val result = pitsA(6) - pitsB(6)

      result match
        case 0 => println("Game ends with a draw")
        case r =>
          if r > 0 then
            println(s"Player A won with advantage of $r points")
          else
            println(s"Player B won with advantage of ${r * -1} points")

    override def showBoard(): Unit =
      println()
      println(s"                 player B                  ")
      println(s"╭-----------------------------------------╮")
      println(s"| ╭--╮ ╭--╮ ╭--╮ ╭--╮ ╭--╮ ╭--╮ ╭--╮ ╭--╮ |")
      println(s"| |  | |${seeds(pitsB(5))}| |${seeds(pitsB(4))}| |${seeds(pitsB(3))}| |${seeds(pitsB(2))}| |${seeds(pitsB(1))}| |${seeds(pitsB(0))}| |  | |")
      println(s"| |  | ╰--╯ ╰--╯ ╰--╯ ╰--╯ ╰--╯ ╰--╯ |  | |")
      println(s"| |${seeds(pitsB(6))}|                               |${seeds(pitsA(6))}| |")
      println(s"| |  | ╭--╮ ╭--╮ ╭--╮ ╭--╮ ╭--╮ ╭--╮ |  | |")
      println(s"| |  | |${seeds(pitsA(0))}| |${seeds(pitsA(1))}| |${seeds(pitsA(2))}| |${seeds(pitsA(3))}| |${seeds(pitsA(4))}| |${seeds(pitsA(5))}| |  | |")
      println(s"| ╰--╯ ╰--╯ ╰--╯ ╰--╯ ╰--╯ ╰--╯ ╰--╯ ╰--╯ |")
      println(s"╰-----------------------------------------╯")
      println(s"                 player A                  ")
      println()

    private def seeds(seeds: Int) =
      seeds match
        case n if n < 0 => throw new Exception("Should not happened !")
        case n if n < 10 => s"0$n"
        case n if n < 100 => s"$n"
        case n if n >= 100 => throw new Exception("Should not happened !")

    override def firstAvailableMoveA(): Int =
      var nextMove = -1
      for i <- 0 to 5 do
        if pitsA(5 - i) != 0 then nextMove = 5 - i
      nextMove

    override def firstAvailableMoveB(): Int =
      var nextMove = -1
      for i <- 0 to 5 do
        if pitsB(5 - i) != 0 then nextMove = 5 - i
      nextMove

    override def checkMoveA(move: Int): Int =
      if pitsA(move) != 0 then
        move
      else
        println("Chosen pit have no seeds in it! Choose again !")
        -1

    override def checkMoveB(move: Int): Int =
      if pitsB(move) != 0 then
        move
      else
        println("Chosen pit have no seeds in it! Choose again !")
        -1

    override def nextMoveA(move: Int): Boolean =
      val seeds = pitsA(move)
      pitsA(move) = 0
      var pitIndex = move + 1

      for i <- 1 to seeds do
        if pitIndex < 7 then
          pitsA(pitIndex) += 1
          pitIndex += 1
        else
          pitsB(pitIndex - 7) += 1
          pitIndex += 1
        pitIndex %= 13

      pitIndex -= 1

      if pitIndex >= 0 && pitIndex < 6 && pitsA(pitIndex) == 1 then
        pitsA(pitIndex) += pitsB(5 - pitIndex)
        pitsB(5 - pitIndex) = 0

      pitIndex == 6

    override def nextMoveB(move: Int): Boolean =
      val seeds = pitsB(move)
      pitsB(move) = 0
      var pitIndex = move + 1

      for i <- 1 to seeds do
        if pitIndex < 7 then
          pitsB(pitIndex) += 1
          pitIndex += 1
        else
          pitsA(pitIndex - 7) += 1
          pitIndex += 1
        pitIndex %= 13

      pitIndex -= 1

      if pitIndex >= 0 && pitIndex < 6 && pitsB(pitIndex) == 1 then
        pitsB(pitIndex) += pitsA(5 - pitIndex)
        pitsA(5 - pitIndex) = 0

      pitIndex == 6

    def result(): Int =
      pitsA(6) - pitsB(6)

    def seedsA(): Int =
      var seedsA = 0
      for i <- 0 to 5 do
        seedsA += pitsA(i)

      seedsA

    def seedsB(): Int =
      var seedsB = 0
      for i <- 0 to 5 do
        seedsB += pitsB(i)

      seedsB
}
