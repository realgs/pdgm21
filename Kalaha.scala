class Kalaha:
  val numOfHouses = 6

//  def move(player1: Array[Int], player2: Array[Int], player1Turn: Boolean, houseNumber: Int): (Array[Int], Array[Int], Int, Int) =
//    var startingPlayerHouses = if player1Turn then player1 else player2
//    var otherPlayerHouses = if player1Turn then player2 else player1
//    var startingPlayerPointsGained = 0
//    var otherPlayerPointsGained = 0
//    var index = houseNumber
//    var startingPlayerSide = true
//    val iterations = startingPlayerHouses(houseNumber)
//
//    startingPlayerHouses(index) = 0
//    for(i <- 0 until iterations)
//      index = index + 1
//      (index, startingPlayerSide) match
//        case (6, _) =>
//          index = -1
//          if startingPlayerSide then
//            startingPlayerPointsGained = startingPlayerPointsGained + 1
//          else
//            otherPlayerPointsGained = otherPlayerPointsGained + 1
//          startingPlayerSide = !startingPlayerSide
//        case (_, true) =>
//          if i == iterations - 1 && startingPlayerHouses(index) == 0 && otherPlayerHouses(numOfHouses - index - 1) != 0 then
//            startingPlayerPointsGained = startingPlayerPointsGained + otherPlayerHouses(numOfHouses - index - 1) + 1
//            otherPlayerHouses(numOfHouses - index - 1) = 0
//          else
//            startingPlayerHouses(index) = startingPlayerHouses(index) + 1
//        case (_, false) => otherPlayerHouses(index) = otherPlayerHouses(index) + 1
//
//    val player1PointsGained = if player1Turn then startingPlayerPointsGained else otherPlayerPointsGained
//    val player2PointsGained = if player1Turn then otherPlayerPointsGained else startingPlayerPointsGained
//    (player1, player2, player1PointsGained, player2PointsGained)

  def computePointsGained(player1: Array[Int], player2: Array[Int], player1Turn: Boolean, houseNumber: Int): Int =
    var startingPlayerHouses = if player1Turn then player1 else player2
    var otherPlayerHouses = if player1Turn then player2 else player1
    var index = houseNumber
    var startingPlayerSide = true
    var pointsGained = 0
    val iterations = startingPlayerHouses(houseNumber)

    startingPlayerHouses(index) = 0
    for(i <- 0 until iterations)
      index = index + 1

      if index == 6 then
        startingPlayerSide match
          case true =>
            index = -1
            pointsGained = pointsGained + 1
          case false => index = 0
        startingPlayerSide = !startingPlayerSide

      if startingPlayerSide && i == iterations - 1 && startingPlayerHouses(index) == 0 then
        pointsGained = pointsGained + otherPlayerHouses(numOfHouses - index - 1) + 1

    pointsGained

  def computeHousesState(player1: Array[Int], player2: Array[Int], player1Turn: Boolean, houseNumber: Int): (Array[Int], Array[Int]) =
    var startingPlayerHouses = if player1Turn then player1 else player2
    var otherPlayerHouses = if player1Turn then player2 else player1
    var index = houseNumber
    var startingPlayerSide = true
    val iterations = startingPlayerHouses(houseNumber)

    startingPlayerHouses(index) = 0
    for(i <- 0 until iterations)
      index = index + 1

      if index == 6 then
        startingPlayerSide match
          case true => index = -1
          case false => index = 0
        startingPlayerSide = !startingPlayerSide

      if index != -1 then
        startingPlayerSide match
          case true =>
            if i == iterations - 1 && startingPlayerHouses(index) == 0 && otherPlayerHouses(numOfHouses - index - 1) != 0 then
              otherPlayerHouses(numOfHouses - index - 1) = 0
            else
              startingPlayerHouses(index) = startingPlayerHouses(index) + 1
          case false => otherPlayerHouses(index) = otherPlayerHouses(index) + 1

    (player1, player2)

  def isGameOver(player1: Array[Int], player2: Array[Int]): Boolean =
    var state1 = 0
    var state2 = 0
    for(i <-0 until player1.length)
      state1 = state1 + player1(i)
      state2 = state2 + player2(i)
    state1 == 0 || state2 == 0

  def chooseMove(player1: Array[Int], player2: Array[Int], p1Gained: Int, p2Gained: Int, depth: Int, player1Turn: Boolean): (Int, Int) =
    if depth == 0 || isGameOver(player1, player2) then (p1Gained - p2Gained, -1)
    else if player1Turn then
      var maxEval = Double.NegativeInfinity
      var bestIdx = -1
      for(i <-0 until numOfHouses)
        val housesAfterChoosingI = computeHousesState(player1.clone(), player2.clone(), true, i)
        val pointsGainedAfterChoosingI = computePointsGained(player1.clone(), player2.clone(), true, i)
        val eval = chooseMove(housesAfterChoosingI._1, housesAfterChoosingI._2, p1Gained + pointsGainedAfterChoosingI, p2Gained, depth - 1, false)._1
        if eval >= maxEval && player1(i) != 0 then
          maxEval = eval
          bestIdx = i
      (maxEval.toInt, bestIdx)
    else
      var minEval = Double.PositiveInfinity
      var bestIdx = -1
      for(i <-0 until numOfHouses)
        val housesAfterChoosingI = computeHousesState(player1.clone(), player2.clone(), false, i)
        val pointsGainedAfterChoosingI = computePointsGained(player1.clone(), player2.clone(), false, i)
        val eval = chooseMove(housesAfterChoosingI._1, housesAfterChoosingI._2, p1Gained, p2Gained + pointsGainedAfterChoosingI, depth - 1, true)._1
        if eval <= minEval && player2(i) != 0 then
          minEval = eval
          bestIdx = i
      (minEval.toInt, bestIdx)

//  def chooseMove(player1: Array[Int], player2: Array[Int], p1Gained: Int, p2Gained: Int, depth: Int, player1Turn: Boolean): (Int, Int) =
//    if depth == 0 || isGameOver(player1, player2) then (p1Gained - p2Gained, -1)
//    else if player1Turn then
//      var maxEval = Double.NegativeInfinity
//      var bestIdx = -1
//      for(i <-0 until numOfHouses)
//        val copiedP1 = player1.clone()
//        val copiedP2 = player2.clone()
//        val afterChoosingI = move(copiedP1, copiedP2, true, i)
//        val eval = chooseMove(afterChoosingI._1, afterChoosingI._2, afterChoosingI._3, afterChoosingI._4, depth - 1, false)._1
//        if eval >= maxEval && player1(i) != 0 then
//          maxEval = eval
//          bestIdx = i
//      (maxEval.toInt, bestIdx)
//    else
//      var minEval = Double.PositiveInfinity
//      var bestIdx = -1
//      for(i <-0 until numOfHouses)
//        val copiedP1 = player1.clone()
//        val copiedP2 = player2.clone()
//        val afterChoosingI = move(copiedP1, copiedP2, false, i)
//        val eval = chooseMove(afterChoosingI._1, afterChoosingI._2, afterChoosingI._3, afterChoosingI._4, depth - 1, true)._1
//        if eval <= minEval && player2(i) != 0 then
//          minEval = eval
//          bestIdx = i
//      (minEval.toInt, bestIdx)

