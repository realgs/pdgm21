import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class LogicEngine(val game: Kalaha, val playerId: Int) {

  var moves: List[Int] = List[Int]()
  val decisions = new MyTree(6, game)

  def makeMove(): Int =

    if !decisions.getNode(moves).hasKids then {
    decisions.getNode(moves).setValue(game.getScore(playerId), game)

    simulate()
    }
    var bestMove = -1
    var bestScore = -100
    val simResult = decisions.getNode(moves).findPaths()
    simResult.foreach(elem =>
      if elem._1._1 > bestScore && game.getField(playerId)(elem._2.head) != 0 then {
        bestScore = elem._1._1
        bestMove = elem._2.head
      })
    moves = moves :+ bestMove
    bestMove


  def simulate(): Unit =
    def simulateMove(path: List[Int]): Unit =
      /*
      val move0 = simulateIteration(0, moves:::path)
      val move1 = simulateIteration(1, moves:::path)
      val move2 = simulateIteration(2, moves:::path)
      val move3 = simulateIteration(3, moves:::path)
      val move4 = simulateIteration(4, moves:::path)
      val move5 = simulateIteration(5, moves:::path)
      */

      val move0 = Await.result(Future(simulateIteration(0, moves:::path)), Duration.Inf)
      val move1 = Await.result(Future(simulateIteration(1, moves:::path)), Duration.Inf)
      val move2 = Await.result(Future(simulateIteration(2, moves:::path)), Duration.Inf)
      val move3 = Await.result(Future(simulateIteration(3, moves:::path)), Duration.Inf)
      val move4 = Await.result(Future(simulateIteration(4, moves:::path)), Duration.Inf)
      val move5 = Await.result(Future(simulateIteration(5, moves:::path)), Duration.Inf)

      if move0 == playerId then simulateMove(path:+0)
      if move1 == playerId then simulateMove(path:+1)
      if move2 == playerId then simulateMove(path:+2)
      if move3 == playerId then simulateMove(path:+3)
      if move4 == playerId then simulateMove(path:+4)
      if move5 == playerId then simulateMove(path:+5)
    simulateMove(List())


  def simulateIteration(fieldNumber: Int, way: List[Int]): Int =
    val curr = decisions.getNode(way)
    val simulationGame = curr.getValue._2.copy()
    val whoseNext = simulationGame.move(playerId, fieldNumber)
    curr.addNewKidAt((simulationGame.getScore(playerId), simulationGame),fieldNumber)
    whoseNext


}
