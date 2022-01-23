class Game:
  var player1 = Array(3, 4, 2, 8, 1, 3)
  var player2 = Array(6, 5, 4, 3, 2, 1)
  var player1Points = 0
  var player2Points = 0
  var player1Turn = false
  val k = Kalaha()

  def move(houseNumber: Int): Unit =
    // TODO fix extra move after conquestion
    val extraMove = if player1Turn then player1(houseNumber)  % (2*k.numOfHouses+1) == (k.numOfHouses - houseNumber) else player2(houseNumber) % (2*k.numOfHouses+1) == (k.numOfHouses - houseNumber)
    val newHousesState =
      if player1Turn then k.move(player1, player2, true, houseNumber)
      else k.move(player1, player2, false, houseNumber)
    if(!extraMove) player1Turn = !player1Turn
    player1 = newHousesState._1
    player2 = newHousesState._2
    player1Points = player1Points + newHousesState._3
    player2Points = player2Points + newHousesState._4

  def printBoard(): Unit =
    for(i <- player1.reverse)
      print("|" + i)
    println("|")
    for(i <- player2)
      print("|" + i)
    println("|")
    println("Player 1: " + player1Points + "\tPlayer 2: " + player2Points)
