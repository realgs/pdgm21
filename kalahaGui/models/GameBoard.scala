package kalaha.models

import kalaha.models.GameResult.{Draw, FirstWon, GameResult, SecondWon}
import kalaha.models.PlayerEnum.{First, Player, Second}
import kalaha.resource.strings.*
import kalaha.utils.Constants.{NR, PLAYER_HOLES, STONES_AMOUNT}


class GameBoard {

  private val firstPlayerPits: Array[Int] = Array.fill(PLAYER_HOLES+1)(STONES_AMOUNT)
  private val secondPlayerPits: Array[Int] = Array.fill(PLAYER_HOLES+1)(STONES_AMOUNT)

  firstPlayerPits(PLAYER_HOLES) = 0
  secondPlayerPits(PLAYER_HOLES) = 0

  private var turn: Player = First

  private var isSimulation: Boolean = false

  def moveStones(pit: Int): Unit =
    val stonesToMove = getCurrentPlayerPitStonesAmount(pit)
    var hole = pit + 1
    var whos = turn

    turn match
      case First => firstPlayerPits(pit) = 0
      case Second => secondPlayerPits(pit) = 0

    var i = 0
    while(i < stonesToMove){

      if ((hole / (PLAYER_HOLES+1)) >= 1 ) then
        hole = 0
        whos = switchWhos(whos)

      if hole == PLAYER_HOLES && turn != whos then
        i -= 1
      else
        whos match
          case  First =>
            firstPlayerPits(hole) += 1
          case Second =>
            secondPlayerPits(hole) += 1

      i += 1
      hole += 1
    }

    hole -= 1
   // println("hole to: "+ hole)
    tryToCaptureStones(hole, whos)

    if (!isLastStoneInMancala(hole)) then
      switchTurns()



  private def getCurrentPlayerPitStonesAmount(pit: Int): Int =
    if turn == First then
      firstPlayerPits(pit)
    else
      secondPlayerPits(pit)

  private def isLastStoneInMancala(index: Int): Boolean =
    if index == PLAYER_HOLES then
      turn match
        case First =>
          if !isSimulation then
            println("First player has next turn")
        case Second =>
          if !isSimulation then
            println("Second player has next turn")

    index == PLAYER_HOLES


  private def tryToCaptureStones(index: Int, whos: Player): Unit =

    if index != PLAYER_HOLES then
      if turn == First && turn == whos then
        if firstPlayerPits(index) == 1 && secondPlayerPits(PLAYER_HOLES - index - 1) != 0 then
          firstPlayerPits(PLAYER_HOLES) += 1 + secondPlayerPits(PLAYER_HOLES - index - 1)
          secondPlayerPits(PLAYER_HOLES - index -1 ) = 0
          firstPlayerPits(index) = 0
          if !isSimulation then
            println(stones_captured_one)
      else if turn == Second && turn == whos then
        if secondPlayerPits(index) == 1 && firstPlayerPits(PLAYER_HOLES - index - 1) != 0 then
          secondPlayerPits(PLAYER_HOLES) += 1 + firstPlayerPits(PLAYER_HOLES - index - 1)
          firstPlayerPits(PLAYER_HOLES - index -1 ) = 0
          secondPlayerPits(index) = 0
          if !isSimulation then
            println(stones_captured_two)


  def switchTurns(): Unit =
    turn match
      case First => turn = Second
      case Second => turn = First


  private def switchWhos(whos: Player): Player =
    whos match
      case First => Second
      case Second => First


  private def putAllStonesToKahala(): Unit =
    for(i <- 0 until (PLAYER_HOLES))
      firstPlayerPits(PLAYER_HOLES) += firstPlayerPits(i)
      firstPlayerPits(i) = 0

      secondPlayerPits(PLAYER_HOLES) += secondPlayerPits(i)
      secondPlayerPits(i) = 0

  def getWinner(): GameResult =
    if firstPlayerPits(PLAYER_HOLES) > secondPlayerPits(PLAYER_HOLES) then FirstWon
    else if firstPlayerPits(PLAYER_HOLES) == secondPlayerPits(PLAYER_HOLES) then Draw
    else SecondWon


  def isEndOfGame(): Boolean =
    var amountOfStonesNotInMancalaFirst = 0
    var amountOfStonesNotInMancalaSecond = 0
    for(i <- 0 until PLAYER_HOLES ){
      amountOfStonesNotInMancalaFirst += firstPlayerPits(i)
      amountOfStonesNotInMancalaSecond += secondPlayerPits(i)
    }
    if amountOfStonesNotInMancalaFirst == 0 || amountOfStonesNotInMancalaSecond == 0 then
      putAllStonesToKahala()
      true
    else false


  def getScores(): (Int, Int) =
    (firstPlayerPits(PLAYER_HOLES), secondPlayerPits(PLAYER_HOLES))

  def endGame(): (Int, Int) =
    putAllStonesToKahala()
    (firstPlayerPits(PLAYER_HOLES), secondPlayerPits(PLAYER_HOLES))

  def getFirstPlayerScore(): Int =
    firstPlayerPits(PLAYER_HOLES)

  def getSecondPlayerScore(): Int =
    secondPlayerPits(PLAYER_HOLES)

  def getPlayerBoard(player: Player) : List[Int] =
    player match
      case First => firstPlayerPits.toList
      case Second => secondPlayerPits.toList

  def getFirstPlayerPit(index: Int): Int =
    firstPlayerPits(index)

  def getSecondPlayerPit(index: Int): Int =
      secondPlayerPits(index)

  def isMoveValid(pitIndex: Int): Boolean =
    turn match
      case First =>
        if firstPlayerPits(pitIndex) == 0 then false
        else true
      case Second =>
        if secondPlayerPits(pitIndex) == 0 then false
        else true


  def getTurn(): Player =
    turn
  
  override def clone(): GameBoard =
    val clone = new GameBoard()

    for (i <- 0 until PLAYER_HOLES){
      clone.firstPlayerPits(i) = firstPlayerPits(i)
      clone.secondPlayerPits(i) = secondPlayerPits(i)
    }
    clone.turn = turn

    clone

  def changeToSimulationMode(): Unit =
    isSimulation = true

  def printBoard(): Unit =

    turn match
      case Second => printSecondPlyerView()
      case First => printFirstPlayerView()

    for(i <- 0 until PLAYER_HOLES) {
      print("------")
    }
    println()
    print(NR)
    for(i <- 1 until PLAYER_HOLES+1) {
      printf(" %3d|", i)
    }
    println()

  private def printFirstPlayerView(): Unit =
    println()
    println(first_player_turn)

    print("  ")
    for(i <- PLAYER_HOLES-1 to 0 by -1) {
      printf(" %3d|", secondPlayerPits(i))
    }
    println()

    print(s" ${secondPlayerPits(PLAYER_HOLES)} ")
    for(i <- 0 until PLAYER_HOLES) {
      print("     ")
    }
    print(s" ${firstPlayerPits(PLAYER_HOLES)} ")
    println()


    print("  ")
    for(i <- 0 until PLAYER_HOLES) {
      printf(" %3d|", firstPlayerPits(i))
    }
    println()



  private def printSecondPlyerView(): Unit =
    println()
    println(second_player_turn)

    print("  ")
    for(i <- PLAYER_HOLES-1 to 0 by -1) {
      printf(" %3d|", firstPlayerPits(i))
    }
    println()

    print(s" ${firstPlayerPits(PLAYER_HOLES)} ")
    for(i <- 0 until PLAYER_HOLES) {
      print("     ")
    }
    print(s" ${secondPlayerPits(PLAYER_HOLES)} ")
    println()

    print("  ")
    for(i <- 0 until PLAYER_HOLES) {
      printf(" %3d|", secondPlayerPits(i))
    }
    println()



}
