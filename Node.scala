class Node(private var player:String = "", private var stones:Int = 0, private var ifMyHole: Boolean = false) {
  private var nextInd: Int = -1
  private var oppositeInd: Int = -1

  def nextIndex:Int =  nextInd
  def oppositeIndex:Int = oppositeInd
  
  def setPlayer(name:String) = {
    player = name
  }

  def setStones(number:Int) = {
    stones = number
  }

  def setNext(index:Int) = {
    nextInd = index
  }

  def setOpposite(index:Int) = {
    oppositeInd = index
  }

  def getPlayer(): String = {
    player
  }

  def getStones(): Int = {
    stones
  }
  
  def getIfMyHole() = {
    ifMyHole
  }
  
  def showNode(): Unit = {
    println("PLAYER: " + player)
    println("Stones in hole: "+stones)
  }


}
