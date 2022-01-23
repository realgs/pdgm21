class AIPlayer(private val id: Int, private val game: Kalaha) extends Player(id) {

  private val logicEngine = LogicEngine(game, id)

  override def getId: Int = id

  override def makeMove(time: Int): Int = {

    logicEngine.makeMove(time: Int)

  }
  
  override def getEnemyMove(move: Int): Unit = logicEngine.updateEnemyMove(move)

}
