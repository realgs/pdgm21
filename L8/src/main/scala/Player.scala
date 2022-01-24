class Player {
  protected var houses: Array[Int] = Array(4, 4, 4, 4, 4, 4)
  protected var base: Int = 0
  
  def getHouses : Array[Int] = houses
  def getBase : Int = base

  // returned -1 means free move for the player
  // returned -2 means capture of enemy seeds on parallel house
  def takeFrom(houseNumber : Int, seeds : Int, startingHouse : Int) : (Int, Int) = {
    if houseNumber < 0 || houseNumber >= houses.length then throw new IllegalArgumentException("Invalid house number")
    else {
      if seeds == 0 then return (0, 0)
      else {
        var leftSeeds = seeds
        for (i <- startingHouse until houses.length) {
          houses.update(i, houses(i) + 1)
          leftSeeds = leftSeeds - 1
          if leftSeeds == 0 then {
            if houses(i) == 1 then return (-2, i)
            else return (0, i - 1)
          }
        }
        if leftSeeds != 0 then {
          base = base + 1
          leftSeeds = leftSeeds - 1
        }
        if leftSeeds == 0 then return (-1, -1)
        else return (leftSeeds, -1)
      }
    }
  }

  def takeAllFrom(houseNumber : Int) : (Int, Int) = {
    val toReturn = takeFrom(houseNumber, houses(houseNumber), houseNumber + 1)
    houses(houseNumber) = 0
    toReturn
  }

  def takeFromRivalMove(seedsNumber : Int) : (Int, Int) = {
    if seedsNumber != 0 then takeFrom(0, seedsNumber, 0)
    else (0, -1)
  }
  
  def addToBase(seedsNumber : Int): Unit = {
    base = base + seedsNumber
  }

  def clearHouse(houseNumber : Int) : Int = {
    val toReturn = houses(houseNumber)
    houses(houseNumber) = 0
    toReturn
  }

  def printHouses() : Unit = {
    houses.foreach(house => printf(s"$house  "))
  }
  
  def canMakeMove() : Boolean = {
    for(house <- houses) {
      if house != 0 then return true
    }
    return false
  }
  
  def isPossible(move : Int): Boolean = {
    houses(move - 1) != 0
  }

  def printHousesReversed() : Unit = {
    var i = houses.length - 1
    while(i != -1) {
      print(houses(i) + "  ")
      i = i -1
    }

  }
  
  def gameEnd() : Unit = {
    for(i <- 0 until (houses.length - 1)) {
      base = base + clearHouse(i)
    }
  }

  def makeMove() : Int = {
    if !canMakeMove() then Int.MinValue
    else {
      var choice = -1
      try {
        choice = scala.io.StdIn.readInt()
      } catch {
        case e : NumberFormatException => {
          print("WRONG MOVE, CHOOSE AGAIN: ")
          return makeMove()
        }
      }
      if choice <= 0 || choice > 6 || houses(choice - 1) == 0 then {
        print("WRONG MOVE, CHOOSE AGAIN: ")
        return makeMove()
      }
      else choice
    }
  }
  
  def makeCopy() : Player = {
    var newPlayer = Player()
    newPlayer.houses = houses.clone()
    newPlayer.base = base
    newPlayer
  }


}
