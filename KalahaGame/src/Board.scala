class Board(){
  var player1houses = Array(6, 6, 6, 6, 6, 6, 0)
  var player2houses = Array(6, 6, 6, 6, 6, 6, 0)
  val baseIndex = 6

  def setForTest(p1ar: Array[Int], p2ar: Array[Int]): Unit = {
    player1houses = p1ar
    player2houses = p2ar
  }
  def move(player: Int, house: Int): Boolean = {
    var playerHouses = Array[Int]()
    var opponentHouses = Array[Int]()

    player match{
      case 1 =>
        playerHouses = player1houses
        opponentHouses = player2houses
      case 2 =>
        playerHouses = player2houses
        opponentHouses = player1houses
      case _ =>
    }
    var nextMove = false

    if(house < 1 || house > 6) then
      throw new IllegalArgumentException("House number must be between 1 and 6")
    else{
      val houseIndex = house - 1
      if playerHouses(houseIndex) == 0 then
        throw new IllegalArgumentException("Cannot take seeds from an empty house")
      else{
        var seeds = playerHouses(houseIndex)
        playerHouses(houseIndex) = 0
        var currentHouse = houseIndex + 1
        while(seeds > 0) {
          while (currentHouse < 7 && seeds > 0) {
            if(seeds == 1 && currentHouse < 6 && playerHouses(currentHouse) == 0) then {
              playerHouses(baseIndex) += opponentHouses(getOppositeHouseIndex(currentHouse))
              opponentHouses(getOppositeHouseIndex(currentHouse)) = 0
            }
            playerHouses(currentHouse) += 1
            seeds -= 1
            currentHouse += 1
          }
          if seeds == 0 && currentHouse == 7 then
            nextMove = true
          else {
            currentHouse = 0
            while (currentHouse < 7 && seeds > 0) {
              opponentHouses(currentHouse) += 1
              seeds -= 1
              currentHouse += 1
            }
            currentHouse = 0

          }
        }
        nextMove
      }
    }

  }
  def getOppositeHouseIndex(myHouseIndex: Int): Int = {
    5 - myHouseIndex
  }

  def isFinished(): Boolean = {
    var sum1 = 0
    var sum2 = 0

    for(i <- 0 to 5){
      sum1 += player1houses(i)
      sum2 += player2houses(i)
    }
//    println(sum1)
//    println(sum2)
    sum1 == 0 || sum2 == 0
  }

  def getFinalScore(): (Int, Int) ={
    var p1score = 0
    var p2score = 0

    for(i <- 0 to 6){
      p1score += player1houses(i)
      p2score += player2houses(i)
    }

    (p1score, p2score)
  }

  def copyBoard(): Board ={
    val copy = new Board()
    copy.player1houses = player1houses.clone()
    copy.player2houses = player2houses.clone()
    copy

  }
  
  def printBoard(): Unit ={
    println("                   Player 2                ")
    println("-----------------------------------------------")
    var counter = player2houses.size - 2
    print("   ")
    while(counter >= 0){
      print(" ( " + player2houses(counter) + " ) ")
      counter -= 1
    }
    println()
    counter = 0
    println("("+player2houses(baseIndex)+")" + "                                          " +"("+ player1houses(baseIndex)+")" )
    print("   ")
    while(counter < player1houses.size - 1){
      print(" ( " + player1houses(counter) + " ) ")
      counter += 1
    }
    println()
    println("-----------------------------------------------")
    println("                   Player 1                ")
  }

}
