class Game(var player1: Array[Int] = Array(4, 4, 4, 4, 4, 4), var player2: Array[Int] = Array(4, 4, 4, 4, 4, 4)):
  var player1Points = 0
  var player2Points = 0
  var player1Turn = false

  val k = Kalaha()

  def move(houseNumber: Int): Unit =
    val extraMove = if player1Turn then player1(houseNumber)  % (2*k.numOfHouses+1) == (k.numOfHouses - houseNumber) else player2(houseNumber) % (2*k.numOfHouses+1) == (k.numOfHouses - houseNumber)
    val newHousesState = k.computeHousesState(player1.clone(), player2.clone(), player1Turn, houseNumber)
    val pointsGained = k.computePointsGained(player1.clone(), player2.clone(), player1Turn, houseNumber)

    player1 = newHousesState._1
    player2 = newHousesState._2
    if player1Turn then player1Points = player1Points + pointsGained
    else player2Points = player2Points + pointsGained

    if(!extraMove) player1Turn = !player1Turn

  def printBoard(): Unit =
    for(i <- player1.reverse)
      print("|" + i)
    println("|")
    for(i <- player2)
      print("|" + i)
    println("|")
    println("Player 1: " + player1Points + "\tPlayer 2: " + player2Points)
    println("---------------------------------")
