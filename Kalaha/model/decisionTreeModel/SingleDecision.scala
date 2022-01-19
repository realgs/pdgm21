package model.decisionTreeModel

import model.GameSpecification

class SingleDecision(_score: Int = 0, _isYourMove: Boolean = true, _arrayOfHouses: Array[Int] = GameSpecification.ARRAYOFHOUSESSTART.clone(), _isEndingMove: Boolean = false):
  def score: Int = _score
  def isYourMove: Boolean = _isYourMove
  def arrayOfHouses: Array[Int] = _arrayOfHouses
  def isEndingMove: Boolean = _isEndingMove

  private def arrToString: String =
    var result: String = ""
    for elem <- arrayOfHouses do result += elem.toString + " "
    result

  override def toString: String = score + " " + isYourMove + ": " + arrToString


