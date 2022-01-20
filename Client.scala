abstract class Client (val ID:String, var hasCurrentTurn: Boolean) {
  
  def getName(): String = ID
  def getHasCurrentTurn(): Boolean = hasCurrentTurn
  def setHasCurrentTurn(bool: Boolean): Unit = { hasCurrentTurn = bool }
  def chooseHouse(game: Game): Int
}