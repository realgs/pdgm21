class Game(val P1: String = "P1", val P2: String = "P2") {

  var gameBoard: Array[House] = setUpNewBoard();
  def getBoard(): Array[House] = gameBoard
  def isHouseEmpty(index: Int): Boolean = if (gameBoard(index).getStonesCount() == 0) then true else false                        //function to check if hole is empty
  def switchTurn(currentID: String): String = if (currentID == P1) then P2 else P1


  def evaluateMove(chosenHouseIndex: Int, ID: String): String = {

    if (chosenHouseIndex == -1) then switchTurn(ID)     //if there was no input
    var stonesCounter = gameBoard(chosenHouseIndex).getStonesCount()
    var turnOwner = ID
    var currentHouseIndex = chosenHouseIndex
    gameBoard(chosenHouseIndex).setStonesCount(0)

    while (stonesCounter > 0) {                                                                                 //simulation of having stones in hand and dropping them one by one
      currentHouseIndex += 1
      if currentHouseIndex == 14 then currentHouseIndex = 0                                                       //in case board array has finished, continue from index 0

      if (ID == P1 && currentHouseIndex == 13 || ID == P2 && currentHouseIndex == 6) {                    //if it's oponent's mancala
        //ignore the opponent's mancala
      } else {                                                                                              //if its not the oponent mancala
        if (stonesCounter == 1) {                                                                           //if its the last stone
          if ((ID == P1 && currentHouseIndex == 6) || (ID == P2 && currentHouseIndex == 13)) {    //if it lands in your mancala

            gameBoard(currentHouseIndex).setStonesCount(gameBoard(currentHouseIndex).getStonesCount() + 1)
            stonesCounter -= 1
            turnOwner = switchTurn(turnOwner)

          } else if (ID == houseOwner(gameBoard(currentHouseIndex).getNextHouseIndex()) && gameBoard(currentHouseIndex).getStonesCount() == 0) {    //if last stone lands on your side in an empty hole
            var oppositeHoleIndex = gameBoard(currentHouseIndex).getOppositeHouseIndex()              // grab opposite hole stones to your mancala
            gameBoard(if (ID == P1) then 6 else 13).setStonesCount(                             //my mancala.setStonesCount
              gameBoard(if (ID == P1) then 6 else 13).getStonesCount()                          // = my mancala.getStonesCount
                + gameBoard(oppositeHoleIndex).getStonesCount()                                     //+ get stones from oppositeHole
                + 1)                                                                                //+ get 1 from current move
            gameBoard(currentHouseIndex).setStonesCount(0)
            gameBoard(oppositeHoleIndex).setStonesCount(0)
            stonesCounter -= 1

          } else {                                                                                        //last stone lands in any hole, just put it and continue
            gameBoard(currentHouseIndex).setStonesCount(gameBoard(currentHouseIndex).getStonesCount() + 1)
            stonesCounter -= 1
          }

        } else {                                                                                          //if its not the last stone
          gameBoard(currentHouseIndex).setStonesCount(gameBoard(currentHouseIndex).getStonesCount() + 1)
          stonesCounter -= 1
        }
      }
    }
      switchTurn(turnOwner)
  }


  def checkIfOutOfStone(ID: String): Boolean = {
    var isSideEmpty = true

    if (ID == P1) {                                       //sum stones on current player's side
      for (i <- 0 to 5) {
        if (gameBoard(i).getStonesCount() != 0) then isSideEmpty = false
      }
      if (isSideEmpty) then moveStonesToMancala(P2)        //move oponent's stones to his mancala

    } else if (ID == P2) {
      for (i <- 7 to 12) {
        if (gameBoard(i).getStonesCount() != 0) then isSideEmpty = false
      }
      if (isSideEmpty) then moveStonesToMancala(P1)
    }
    isSideEmpty
  }


  def moveStonesToMancala(nonEmptySide: String): Unit ={
    var stonesSum = 0
    if (nonEmptySide == P1) {
      for (i <- 0 to 5) {
        stonesSum += gameBoard(i).getStonesCount()
        gameBoard(i).setStonesCount(0)
      }
      gameBoard(6).setStonesCount(gameBoard(6).getStonesCount() + stonesSum)
    } else {
        for (i <- 7 to 12) {
          stonesSum += gameBoard(i).getStonesCount()
          gameBoard(i).setStonesCount(0)
        }
      gameBoard(13).setStonesCount(gameBoard(13).getStonesCount() + stonesSum)
    }
  }



  def anounceWinner(): Unit = {
    if (gameBoard(13).getStonesCount() > gameBoard(6).getStonesCount()) then println("P2 wins! Scores: " + gameBoard(13).getStonesCount() + "\nP1 scores: " + gameBoard(6).getStonesCount())
    else if (gameBoard(13).getStonesCount() < gameBoard(6).getStonesCount()) then println("P1 wins! Scores: " + gameBoard(6).getStonesCount() + "\nP2 scores: " + gameBoard(13).getStonesCount())
    else println("Draw!\n"+ P2 + gameBoard(13).getStonesCount() + "\n" + P2 + gameBoard(6).getStonesCount())

  }

  def setUpNewBoard(): Array[House] = {                                //create board with 14 holes. Index 6 and 13 are mancalas (base)
    var board: Array[House] = new Array[House](14)
    for(i <- 0 to 5) { board(i) = new House(P1, 4) }
    board(6) = new House(P1, 0)
    for(i <- 7 to 12) { board(i) = new House(P2, 4) }
    board(13) = new House(P2, 0)
    for (i <- 0 to 12) { board(i).setNextHouse(i + 1)}
    board(13).setNextHouse(0)

    board(0).setOpposite(12)                                        //set the opposite holes
    board(12).setOpposite(0)
    board(1).setOpposite(11)
    board(11).setOpposite(1)
    board(2).setOpposite(10)
    board(10).setOpposite(2)
    board(3).setOpposite(9)
    board(9).setOpposite(3)
    board(4).setOpposite(8)
    board(8).setOpposite(4)
    board(5).setOpposite(7)
    board(7).setOpposite(5)
    board
  }


  def printCurrentBoard() = {
    var i = 12

    print("*-------------------------------------*\n|   \t ")
    while (i >= 7) {
      print(gameBoard(i).getStonesCount() + "\t ")
      i = i - 1
    }
    print("     |\n|  "+ gameBoard(13).getStonesCount() + "\t\t\t\t\t\t\t  " + gameBoard(6).getStonesCount())
    print("   |\n|   \t ")
    for (i <- 0 to 5) {
      print(gameBoard(i).getStonesCount() + "\t ")
    }
    print("     |\n*-------------------------------------*")
  }



  def alternativeBoardCopy(): Game = {
    var gameCopy: Game = new Game(P1, P2)
    for (i <- 0 to gameBoard.length - 1) {
      gameCopy.getBoard()(i) = new House(gameBoard(i).getOwner(), gameBoard(i).getStonesCount())
    }
    gameCopy
  }

  def houseOwner(index: Int):String = {
    index match {
      case 0 | 1 | 2 | 3 | 4 | 5 | 6 => P1
      case 7 | 8 | 9 | 10 | 11 | 12 | 13 => P2
      case _ => ""
    }
  }


}
