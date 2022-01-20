package model.playerModel

import model.GameSpecification
import model.GameSpecification.{INDEXFIRSTSTORE, INDEXSECONDSTORE, LENGTHOFBOARD}
import model.decisionTreeModel.SingleDecision

abstract class Player(_arrOfHomes: Array[Int] = GameSpecification.PLAYERHOUSESSTART.clone(), private var _score: Int ):
  protected var _name: String = _
  def name: String = _name
  def name_=(newName:String): Unit =
    _name = newName

  def score: Int = _score
  def arrayOfHomes: Array[Int] = _arrOfHomes
  def score_=(newScore: Int): Unit =
    _score = newScore

  def makeMove(moveIndex :Int = 0): (Int, Boolean, Boolean, SingleDecision)
  def countEndingScore(): Int

  protected def checkIfDone(arrayOfHouses: Array[Int]): Int =
    if arrayOfHouses.length == 0 then 1
    else
      var result = true
      for index <- 0 to 5 do
        if arrayOfHouses(index) != 0 then
          result = false

      if !result then
        result = true
        for index <- 7 to 12 do
          if arrayOfHouses(index) != 0 then
            result = false
      if result then 2 else 0

  protected def checkIfCapture(isYourMove: Boolean, index: Int, indexOfYourStore: Int, indexOfEnemiesStore: Int, arrayOfHousesAfterMove: Array[Int]): Boolean =
    ( (isYourMove && index >= 0 && index < indexOfYourStore) || (!isYourMove && index > indexOfEnemiesStore && index < indexOfYourStore) )
      && arrayOfHousesAfterMove(index) == 0 && arrayOfHousesAfterMove((indexOfYourStore + indexOfYourStore - index) % LENGTHOFBOARD) != 0

  protected def nextMove(score: Int, indexStart: Int, isYourMove: Boolean, arrayOfHouses: Array[Int]): SingleDecision =
    if arrayOfHouses(indexStart) == 0 then SingleDecision(-100, false, Array())
    else
      val arrayOfHousesAfterMove = arrayOfHouses.clone()
      var isYourMoveAfterMove = isYourMove

      val indexOfEnemiesStore = if isYourMove then INDEXSECONDSTORE else INDEXFIRSTSTORE
      val indexOfYourStore = if isYourMove then INDEXFIRSTSTORE else INDEXSECONDSTORE

      var amountOfFreeRocks: Int = arrayOfHousesAfterMove(indexStart)
      arrayOfHousesAfterMove(indexStart) = 0

      var index = indexStart + 1
      while amountOfFreeRocks > 1 do
        if index != indexOfEnemiesStore then
          arrayOfHousesAfterMove(index) += 1
          amountOfFreeRocks -= 1

        index = (index + 1) % LENGTHOFBOARD

      if index == indexOfEnemiesStore then index = (index + 1) % LENGTHOFBOARD

      if index == indexOfYourStore then
        arrayOfHousesAfterMove(index) += 1
        amountOfFreeRocks -= 1
        isYourMoveAfterMove = isYourMove

      else if checkIfCapture(isYourMove, index, indexOfYourStore, indexOfEnemiesStore, arrayOfHousesAfterMove) then
        val amountToAdd = arrayOfHousesAfterMove((indexOfYourStore + indexOfYourStore - index) % LENGTHOFBOARD) + amountOfFreeRocks
        arrayOfHousesAfterMove(indexOfYourStore) += amountToAdd
        arrayOfHousesAfterMove(index) = 0
        arrayOfHousesAfterMove((indexOfYourStore + indexOfYourStore - index)% LENGTHOFBOARD) = 0
        isYourMoveAfterMove = !isYourMove

      else
        isYourMoveAfterMove = !isYourMove
        arrayOfHousesAfterMove(index) += 1

      var scoreAfterMove = if isYourMove then arrayOfHousesAfterMove(indexOfYourStore) - arrayOfHousesAfterMove(indexOfEnemiesStore) else arrayOfHousesAfterMove(indexOfEnemiesStore) - arrayOfHousesAfterMove(indexOfYourStore)

      SingleDecision(scoreAfterMove, isYourMoveAfterMove, arrayOfHousesAfterMove, checkIfDone(arrayOfHousesAfterMove) == 2)

