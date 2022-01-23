class Kalaha:
  def move(player1: Array[Int], player2: Array[Int], player1Turn: Boolean, houseNumber: Int): (Array[Int], Array[Int], Int, Int) =
    var startingPlayerHouses = if player1Turn then player1 else player2
    var otherPlayerHouses = if player1Turn then player2 else player1
    var startingPlayerPointsGained = 0
    var otherPlayerPointsGained = 0
    var index = houseNumber
    var startingPlayerSide = true
    val iterations = startingPlayerHouses(houseNumber)

    startingPlayerHouses(index) = 0
    for(i <- 0 until iterations)
      index = index + 1
      (index, startingPlayerSide) match
        case (6, _) =>
          index = -1
          if startingPlayerSide then startingPlayerPointsGained = startingPlayerPointsGained + 1
          else otherPlayerPointsGained = otherPlayerPointsGained + 1
          startingPlayerSide = !startingPlayerSide;
        case (_, true) => startingPlayerHouses(index) = startingPlayerHouses(index) + 1
        case (_, false) => otherPlayerHouses(index) = otherPlayerHouses(index) + 1

    val player1PointsGained = if player1Turn then startingPlayerPointsGained else otherPlayerPointsGained
    val player2PointsGained = if player1Turn then otherPlayerPointsGained else startingPlayerPointsGained
    (player1, player2, player1PointsGained, player2PointsGained)
