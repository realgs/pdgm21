package model.gameModel

import model.playerModel.HumanPlayer
import model.playerModel.{Computer, HumanPlayer}

class SingleGame(private val human: HumanPlayer, private val computer: Computer) extends Game(human, computer):

  private def getInput(): Int =
    print(": ")
    try
      scala.io.StdIn.readInt()
    catch
      case e: NumberFormatException =>
        println("Nie ma takiego wyboru!")
        getInput()

  override def next(isFirstPlayerMove :Boolean): Unit =
    drawBoard(isFirstPlayerMove)
    var chosenIndex = 0
    if isFirstPlayerMove then
      val availableIndexes = human.getAvailableMoves()

      chosenIndex = getInput()
      while !availableIndexes.contains(chosenIndex - 1) do
        chosenIndex = getInput()

    val (moveIndex, isEnding, nextMove, _) = if isFirstPlayerMove then human.makeMove(chosenIndex - 1) else computer.makeMove()
    if !isFirstPlayerMove then Thread.sleep(2000)
    if isFirstPlayerMove then computer.updateAfterEnemiesMove(moveIndex) else human.updateAfterEnemiesMove(moveIndex)

    if !isEnding then next(if nextMove then isFirstPlayerMove else !isFirstPlayerMove) else end()

