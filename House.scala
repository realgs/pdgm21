class House(private var owner: String = "", private var stonesCount: Int = 0) {

  private var nextHouse: Int = -1
  private var oppositeHouse: Int = -1

  def getNextHouseIndex(): Int =  nextHouse
  def getOppositeHouseIndex(): Int = oppositeHouse
  def getOwner(): String = owner
  def getStonesCount(): Int = stonesCount
  
  def setStonesCount(newStonesCount: Int) = { stonesCount = newStonesCount }
  def setNextHouse(newNextHouseIndex: Int) = { nextHouse = newNextHouseIndex }
  def setOpposite(index: Int) = { oppositeHouse = index }



}