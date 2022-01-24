import scala.annotation.tailrec
import scala.concurrent.{Await, Future}
import scala.concurrent.duration.*
import scala.concurrent.ExecutionContext.Implicits.global

class LogicEngine(val game: Kalaha, val playerId: Int) {

  var moves: List[Int] = List[Int]()
  val decisions = new MyTree(6, game)
  var bestMove: Int = 0

  def updateEnemyMove(move: Int): Unit = moves = moves :+ move

  def makeMove(time: Int): Int = {
    val start = System.currentTimeMillis()
    if decisions.getRoot.getValue._3 == 0 then
      if moves.isEmpty then decisions.getRoot.setValue(decisions.getRoot.getValue._1, decisions.getRoot.getValue._2, playerId)
      else {
        decisions.getRoot.setValue(game.getScore(playerId), game, playerId)
        moves = List()
      }
    var root = decisions.getRoot
    moves.foreach(move =>{
      if !root.hasKids then {
        fillChildren()
        root = root.getKids(move)
      }
      else root = root.getKids(move)

    })
    moves = List()
    simulate(time*1000, start)
    decisions.setRoot(decisions.getRoot.getKids(bestMove))
    bestMove
  }

  def fillChildren(): Unit ={
    @tailrec
    def fillKid(curr: Node, restOfMoves: List[Int]):Unit =
      restOfMoves match {
        case List() => ()
        case h::t =>
          simulateIteration(h, curr, curr.getValue._3)
          fillKid(curr.getKids(h),t)
      }


  }

  def updateBestMove(): Unit =
    var choiceMove = (0,0)
    var bestScore = -100
    var i = 0
    decisions.getRoot.bestChoice(playerId).foreach((res, next) =>{
      if game.getField(playerId)(i) != 0 then {
        if res > bestScore then
          bestScore = res
          choiceMove = (i, next)
        else if choiceMove._2 != playerId && res == bestScore && next == playerId then
          bestScore = res
          choiceMove = (i, next)
      }
      i += 1
    })
    bestMove = choiceMove._1

  def simulate(time: Int, start: Long): Unit =

    def simulateMove(curr: Node): Unit =
      if (System.currentTimeMillis() - start) < (time* 3 / 4) then {
        if curr.hasKids then
          curr.getKids.foreach(kid =>{
            simulateMove(kid)
          })
        else
          val f1 = Future(simulateIteration(0,curr, curr.getValue._3))
          val f2 = Future(simulateIteration(1,curr, curr.getValue._3))
          val f3 = Future(simulateIteration(2,curr, curr.getValue._3))
          val f4 = Future(simulateIteration(3,curr, curr.getValue._3))
          val f5 = Future(simulateIteration(4,curr, curr.getValue._3))
          val f6 = Future(simulateIteration(5,curr, curr.getValue._3))
          if ((System.currentTimeMillis() - start) < (time * 4 / 5)) {
            Await.result(f1, Duration.Inf)
            Await.result(f2, Duration.Inf)
            Await.result(f3, Duration.Inf)
            Await.result(f4, Duration.Inf)
            Await.result(f5, Duration.Inf)
            Await.result(f6, Duration.Inf)
          }
          else ()
      }
    while((System.currentTimeMillis() - start) < (time* 4 / 5)){
      simulateMove(decisions.getRoot)
      decisions.getRoot.countSubtree()
      updateBestMove()
    }

  def simulateIteration(fieldNumber: Int, curr: Node, currPlayerId: Int): Unit = {
    val simulationGame = curr.getValue._2.copy()
    val whoseNext = simulationGame.move(currPlayerId, fieldNumber)
    curr.addNewKidAt((simulationGame.getScore(playerId), simulationGame, whoseNext), fieldNumber)
  }

  def showTree(): Unit = decisions.print()
}
