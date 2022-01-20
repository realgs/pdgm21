object Kalaha {
  class Kalaha {
    private val board: Array[Array[Int]] = Array[Array[Int]](Array[Int](4, 4, 4, 4, 4, 4, 0), Array[Int](4, 4, 4, 4, 4, 4, 0))
    private var whoseTurn: Int = 1

    def getWhoseTurn: Int = whoseTurn

    def getOpponentNumber: Int =
      if whoseTurn == 1 then 2
      else 1

    def getPlayerScore(playerNumber: Int): Int = board(playerNumber - 1)(6)

    //return false if pit number was incorrect, expected from 1 to 6 and not empty
    def checkCorrectPitNumber(pitNumber: Int): Boolean =
      if pitNumber < 1 || pitNumber > 6 || board(whoseTurn - 1)(pitNumber - 1) == 0 then false
      else true

    //returns true if player gets another move
    def playerMakesMove(pitNumber: Int): Boolean =
      val seedsNumber = board(whoseTurn - 1)(pitNumber - 1)
      board(whoseTurn - 1)(pitNumber - 1) = 0
      if !moveSeeds(seedsNumber, pitNumber, whoseTurn) then
        whoseTurn = getOpponentNumber
        false
      else true


    //returns true if last seed was put into player's store, player gets another move
    def moveSeeds(seedsLeft: Int, pitNumber: Int, sideOfBoard: Int): Boolean =
      board(sideOfBoard - 1)(pitNumber) += 1

      //last seed was put into player's store, player gets another move
      if seedsLeft == 1 && sideOfBoard == whoseTurn && pitNumber == 6 then
        true

      //last seed was put into a pit on player's side of the board
      else if seedsLeft == 1 && sideOfBoard == whoseTurn then
        if board(whoseTurn - 1)(pitNumber) == 1 then
          tryToCaptureOpponentsSeeds(pitNumber) //the pit was empty
        false

      //last seed was put into a pit on opponent's side of the board
      else if seedsLeft == 1 && sideOfBoard != whoseTurn then
        false

      //seed was put into player's store
      else if seedsLeft > 1 && sideOfBoard == whoseTurn && pitNumber == 6 then
        moveSeeds(seedsLeft - 1, 0, getOpponentNumber)

      //seed was put into a pit on player's side of the board
      else if seedsLeft > 1 && sideOfBoard == whoseTurn then
        moveSeeds(seedsLeft - 1, pitNumber + 1, whoseTurn)

      //seed was put into last pit on opponent's side of the board
      else if seedsLeft > 1 && sideOfBoard != whoseTurn && pitNumber == 5 then
        moveSeeds(seedsLeft - 1, 0, whoseTurn)

      //seed was put into any other than last pit on opponent's side of the board
      else if seedsLeft > 1 && sideOfBoard != whoseTurn then
        moveSeeds(seedsLeft - 1, pitNumber + 1, getOpponentNumber)

      else false


    def tryToCaptureOpponentsSeeds(pitNumber: Int): Unit =
      val opponentPitNumber = 6 - (pitNumber + 1)
      if board(getOpponentNumber - 1)(opponentPitNumber) > 0 then
        board(whoseTurn - 1)(6) += (1 + board(getOpponentNumber - 1)(opponentPitNumber))
        board(whoseTurn - 1)(pitNumber) = 0
        board(getOpponentNumber - 1)(opponentPitNumber) = 0;


    //return true if one player has no seeds left in their pits
    def checkGameOver: Boolean =
      var sumPlayer1 = 0
      var sumPlayer2 = 0
      for (i <- 0 to 5)
        sumPlayer1 += board(0)(i)
        sumPlayer2 += board(1)(i)

      if sumPlayer1 == 0 || sumPlayer2 == 0 then true
      else false


    def copyBoard: Kalaha =
      val copy = new Kalaha
      copy.whoseTurn = whoseTurn
      for (i <- 0 to 6)
        copy.board(0)(i) = board(0)(i)
        copy.board(1)(i) = board(1)(i)
      copy


    def printBoard: Unit =
      println("Player 2 side:")
      print("    |")
      for (i <- 5 to 0 by -1)
        print(" " + board(1)(i) + " |")
      println("\n| " + board(1)(6) + " |")
      println("                            | " + board(0)(6) + " |")
      print("    |")
      for (i <- 0 to 5)
        print(" " + board(0)(i) + " |")
      println("\n                    Player 1 side\n")


    def printResults: Unit =
      var sumPlayer1 = board(0)(6)
      var sumPlayer2 = board(1)(6)
      for (i <- 0 to 5)
        sumPlayer1 += board(1)(i)
        sumPlayer2 += board(0)(i)

      if sumPlayer1 > sumPlayer2 then
        System.out.println("GAME OVER - PLAYER 1 WON")
        System.out.println("Player 1 points: " + sumPlayer1)
        System.out.println("Player 2 points: " + sumPlayer2)
      else if sumPlayer1 < sumPlayer2 then
        System.out.println("GAME OVER - PLAYER 2 WON")
        System.out.println("Player 1 points: " + sumPlayer1)
        System.out.println("Player 2 points: " + sumPlayer2)
      else
        System.out.println("GAME OVER - DRAW")
        System.out.println("Player 1 points: " + sumPlayer1)
        System.out.println("Player 2 points: " + sumPlayer2)

  }
}