class LogicEngine(val game: Kalaha, val playerId: Int) {

  var moves = List[Int]()
  val decisions =new Tree(6)
  val simulations = new Array[Kalaha](6)

  def makeMove(): Int =




    0

  def simulateMove(fieldNumber: Int): Unit =
    simulations(fieldNumber - 1) = game.copy()
    simulations(fieldNumber - 1).move(fieldNumber-1, playerId)
    decisions.getNode(moves).addNewKidAt(simulations(fieldNumber - 1).getScore(playerId), fieldNumber - 1)

  def addEnemyMove(move: Int): Unit = moves = moves :+ move

}
