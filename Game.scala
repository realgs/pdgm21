class Game:
  var player1 = Array(4, 4, 4, 4, 4, 4)
  var player2 = Array(4, 4, 4, 4, 4, 4)
  var player1Points = 0
  var player2Points = 0
  val k = Kalaha()

  def move(player1Turn: Boolean, houseNumber: Int): Unit =
    val newHousesState =
      if player1Turn then k.move(player1, player2, true, houseNumber)
      else k.move(player1, player2, false, houseNumber)
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
