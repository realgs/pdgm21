class AIPlayer(id: Int, game: Kalaha) extends Player(id) {

  private val logicEngine = LogicEngine(game, id)

  override def getId: Int = id
  
  override def makeMove(): Int = {

    logicEngine.makeMove()
    
  }

}
