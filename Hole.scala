package Game

class Hole(private var pebbles : Int, private var base : Boolean, private var player : Int, private var opposite : Int = -1)
{
  def getPebbles : Int = pebbles
  def setPebbles(newPebbles : Int) : Unit =
    pebbles = newPebbles
  def incrementPebbles : Unit =
    pebbles += 1

  def getBase : Boolean = base
  def setBase(newBase : Boolean) : Unit =
    base = newBase

  def getPlayer : Int = player
  def setPlayer(newPlayer : Int) : Unit =
    player = newPlayer

  def isEmpty : Boolean =
    if pebbles == 0 then true
    else false

  def getOpposite : Int = opposite
  def setOpposite(newOpp : Int)  : Unit=
    opposite = newOpp
  
}

