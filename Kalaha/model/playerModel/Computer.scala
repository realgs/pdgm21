package model.playerModel

import model.GameSpecification.{AILEVEL, ARRAYOFHOUSESSTART, INDEXFIRSTSTORE, PLAYERHOUSESSTART}
import model.playerModel.Player
import model.decisionTreeModel.{DecisionTree, Empty, Node, SingleDecision}

import scala.annotation.tailrec
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class Computer(_arrOfHomes: Array[Int] = PLAYERHOUSESSTART.clone(), _score: Int = 0, isStartingGame: Boolean = true) extends Player(_arrOfHomes, _score):
  _name = "PC"

  private var _decisionTree = makeTree(isYourMove = isStartingGame)
  def decisionTree: DecisionTree[SingleDecision] = _decisionTree
  def decisionTree_=(newTree: DecisionTree[SingleDecision]): Unit =
    _decisionTree = newTree


  private def makeTree(score: Int = 0, isYourMove: Boolean = true, arrayOfHouses: Array[Int] = ARRAYOFHOUSESSTART.clone(), isDone: Int = 0): DecisionTree[SingleDecision] =
    if isDone == 1 then Empty
    else
      val moves: Seq[SingleDecision] = for index <- 0 to 5 yield nextMove(score, if isYourMove then index else index + 7, isYourMove, arrayOfHouses)
      Node(new SingleDecision(score, isYourMove, arrayOfHouses, isDone == 2), (for move <- moves yield () => {makeTree(move.score, move.isYourMove, move.arrayOfHouses, checkIfDone(move.arrayOfHouses))}).toList)

  private def isCurrentBestDecision(bestDecision: SingleDecision, currentDecision: SingleDecision): Boolean =
    (bestDecision.score < currentDecision.score) || (currentDecision.isYourMove && !bestDecision.isYourMove) || (currentDecision.isYourMove && bestDecision.isYourMove && bestDecision.score < currentDecision.score )

  private def minFinal(decisions: List[() => DecisionTree[SingleDecision]]): (Int, Int) =
    var minValue: (Int, Int) = ({if decisions.head() == Empty then 100 else {val Node(decision, _): DecisionTree[SingleDecision] = decisions.head(); decision.score}}, 0)
    var dec = decisions.tail

    var index = 1
    while dec != Nil do
      val current: (Int, Int) = ({if dec.head() == Empty then 100 else {val Node(decision, _): DecisionTree[SingleDecision] = dec.head(); decision.score}}, index)
      if minValue._1 > current._1 then
        minValue = current
      index += 1
      dec = dec.tail
    minValue

  private def min(decisions: List[(Int, Int)]): (Int, Int) =
    var minValue: (Int, Int) = (decisions.head._1, 0)
    var dec = decisions.tail

    var index = 1
    while dec != Nil do
      val current: (Int, Int) = (dec.head._1, index)
      if minValue._1 > current._1 then
        minValue = current
      index += 1
      dec = dec.tail
    minValue

  private def maxFinal(decisions: List[() => DecisionTree[SingleDecision]]): (Int, Int) =
    var maxValue: (Int, Int) = ({if decisions.head() == Empty then -100 else {val Node(decision, _): DecisionTree[SingleDecision] = decisions.head(); decision.score}}, 0)
    var dec = decisions.tail

    var index = 1
    while dec != Nil do
      val current: (Int, Int) = ({if dec.head() == Empty then -100 else {val Node(decision, _): DecisionTree[SingleDecision] = dec.head(); decision.score}}, index)
      if maxValue._1 < current._1 then
        maxValue = current
      index += 1
      dec = dec.tail
    maxValue

  private def max(decisions: List[(Int, Int)]): (Int, Int) =
    var maxValue: (Int, Int) = (decisions.head._1, 0)
    var dec = decisions.tail

    var index = 1
    while dec != Nil do
      val current: (Int, Int) = (dec.head._1, index)
      if maxValue._1 < current._1 then
        maxValue = current
      index += 1
      dec = dec.tail
    maxValue

  private def findBestMove(decisions: List[() => DecisionTree[SingleDecision]], depth: Int, isMax: Boolean): (Int, Int) =
    if depth == 0 then
      if isMax then maxFinal(decisions)
      else minFinal(decisions)
    else
      var index = -1
      val scores =
        for decision <- decisions yield
          index += 1
          val child = decision() match
            case Empty => (SingleDecision(-100, _arrayOfHouses = Array(), _isEndingMove = false), Nil)
            case Node(element, children) => (element, children)

          if child._1.isEndingMove then (child._1.score, index)
          else if child._2 != Nil then
            if depth > AILEVEL - 4 then
              val future = Future(findBestMove(child._2, depth - 1, child._1.isYourMove))
              Await.result(future, Duration.Inf)
            else findBestMove(child._2, depth - 1, child._1.isYourMove)
          else if child._2 == Nil && isMax then (-100, index) else (100, index)

//      println(scores)
      if isMax then max(scores)
      else min(scores)

  private def findBestMove(tree: DecisionTree[SingleDecision]): Int =
    tree match
      case Empty => -1
      case Node(_, children) => findBestMove(children, AILEVEL, true)._2

  @tailrec
  private def findInTree(decisions: List[() => DecisionTree[SingleDecision]], index: Int): DecisionTree[SingleDecision] =
    if decisions == Nil then Empty
    else if decisions.head() == Empty then findInTree(decisions.tail, index - 1)
    else if index == 0 && decisions.head() != Empty then decisions.head()
    else findInTree(decisions.tail, index - 1)

  private def findInTree(tree:  DecisionTree[SingleDecision], index: Int): DecisionTree[SingleDecision] =
    tree match
      case Empty => Empty
      case Node(_, children) => findInTree(children, index)

  private def extractHomesFromBoard(board: SingleDecision): Unit =
    if board.arrayOfHouses.length != 0 then
      for index <- 0 until INDEXFIRSTSTORE do
        arrayOfHomes(index) = board.arrayOfHouses(index)

  def countEndingScore(): Int =
    var result = score
    for home <- arrayOfHomes do
      result = result + home
    result

  override def makeMove(moveIndex :Int = 0): (Int, Boolean, Boolean, SingleDecision) =
      val indexOfSubTree = findBestMove(decisionTree)
      decisionTree = findInTree(decisionTree, indexOfSubTree)
      val Node(boardAfterMove, _) = decisionTree
      extractHomesFromBoard(boardAfterMove)
      score = boardAfterMove.arrayOfHouses(INDEXFIRSTSTORE)
      (indexOfSubTree, boardAfterMove.isEndingMove, boardAfterMove.isYourMove, boardAfterMove)

  def updateAfterEnemiesMove(indexOfSubTree: Int): Unit =
    val decTree = decisionTree match
      case Empty => Nil
      case Node(_, children) => children

    decisionTree = findInTree(decTree, indexOfSubTree)
    var newScore = 0
    decisionTree match
      case Empty => extractHomesFromBoard(SingleDecision(-100, _arrayOfHouses = Array(), _isEndingMove = true))
      case Node(elem, _) => extractHomesFromBoard(elem); newScore = elem.arrayOfHouses(INDEXFIRSTSTORE)

    score = newScore



