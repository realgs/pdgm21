import java.util.Scanner
import scala.util.Random

class Kalah(val playerName: String = "Player1")
{
  private var board: Array[Int] = createBoard()
  private var simulatedBoard: Array[Int] = createBoard()
  private var hasEnded: Boolean = false
  private var scanner: Scanner = new Scanner(System.in)
  private var lastField: Int = 0
  private var lastFieldSimulated: Int = 0
  private var random: Random = new Random()

  private var predictedPoints = Array.ofDim[(Int, Int)](13, 13)

  def createBoard(): Array[Int] =
    var board: Array[Int] = new Array[Int](14)  //0-6: PLAYER; 7-13 COMPUTER; 6 - PLAYER BASE; 13 - COMPUTER BASE

    for(i <- 0 to 5)
      board(i) = 6

    for(i <- 7 to 12)
      board(i) = 6

    board

  def getCurrentBoardStatus(): Unit =
    println("\nBOARD STATUS: ")
    printf("    %5s%5s%5s%5s%5s%5s%n", "(12)", "(11)", "(10)", "(9)", "(8)", "(7)")
    printf("(13)%4d%5d%5d%5d%5d%5d   (6)%n", board(12), board(11), board(10), board(9), board(8), board(7))
    printf("%3d    --------------------------%5d%n", board(13), board(6))
    printf("   %5d%5d%5d%5d%5d%5d%n", board(0), board(1), board(2), board(3), board(4), board(5))
    printf("    %5s%5s%5s%5s%5s%5s%n", "(0)", "(1)", "(2)", "(3)", "(4)", "(5)")

  def makeMove(firstHole: Int): Unit =
    for(i <- 1 to board(firstHole))
      board((firstHole + i) % 14) += 1

    lastField = (firstHole + board(firstHole)) % 14

    board(firstHole) = 0

    //getCurrentBoardStatus()

  def makeSimulatedMove(firstHole: Int): Unit =
    for(i <- 1 to simulatedBoard(firstHole))
      simulatedBoard((firstHole + i) % 14) += 1

    lastFieldSimulated = (firstHole + simulatedBoard(firstHole)) % 14

    simulatedBoard(firstHole) = 0

  //ALGORYTM ZACHOWANIA SIE AI
  def makeMoveAI(whichHalf: Int): Unit =              //0 - UPPER HALF; 1 - LOWER HALF
    if whichHalf == 0 then
      for(i <- 7 to 12)
        for(j <- 0 to 5)
          predictedPoints(i)(j) = simulateMove(i, j, whichHalf)

    if whichHalf == 1 then
      for(i <- 0 to 5)
        for(j <- 7 to 12)
          predictedPoints(i)(j) = simulateMove(i, j, whichHalf)

    var maxDifference: Int = Integer.MIN_VALUE
    var maxI: Int = Integer.MIN_VALUE

    if whichHalf == 0 then
      maxI = random.nextInt(6) + 7
    else
      maxI = random.nextInt(6)

    while(board(maxI) == 0)
      if whichHalf == 0 then
        maxI = random.nextInt(6) + 7
      else
        maxI = random.nextInt(6)

    if whichHalf == 0 then
      for(i <- 7 to 12)
        for(j <- 0 to 5)
          if (predictedPoints(i)(j)._2 - predictedPoints(i)(j)._1) > maxDifference && simulatedBoard(i) != 0 then
            maxDifference = predictedPoints(i)(j)._2 - predictedPoints(i)(j)._1
            maxI = i

    if whichHalf == 1 then
      for(i <- 0 to 5)
        for(j <- 7 to 12)
          if (predictedPoints(i)(j)._1 - predictedPoints(i)(j)._2) > maxDifference && simulatedBoard(i) != 0 then
            maxDifference = predictedPoints(i)(j)._1 - predictedPoints(i)(j)._2
            maxI = i

    print("\nMax Difference: " + maxDifference)

    makeMove(maxI)

    predictedPoints.empty

    println("\nAI is making a move from " + maxI)

    getCurrentBoardStatus()

  def simulateMove(firstHoleAI: Int, firstHolePlayer: Int, whichHalf: Int): (Int, Int) =
    var stonesInHole: Int = simulatedBoard(firstHoleAI)

    makeSimulatedMove(firstHoleAI)

    checkIfZeroOnFieldSimulation(whichHalf, lastFieldSimulated)

    makeSimulatedMove(firstHolePlayer)

    if whichHalf == 0 then
      checkIfZeroOnFieldSimulation(1, lastFieldSimulated)
    else
      checkIfZeroOnFieldSimulation(0, lastFieldSimulated)

    var pointsAfterMove: (Int, Int) = (simulatedBoard(6), simulatedBoard(13))

    for(i <- 0 to 13)
      simulatedBoard(i) = board(i)

    pointsAfterMove

  def playAIvsAI(): Unit =
    println("AI VS AI MODE")

    var whoStarts: Int = random.nextInt(2)

    if whoStarts == 0 then makeMoveAI(0)

    while(lastField == 13 && (board(7) + board(8) + board(9) + board(10) + board(11) + board(12)) != 0)
      makeMoveAI(0)

    while(!hasEnded)
      makeMoveAI(1)

      while(lastField == 6 && (board(0) + board(1) + board(2) + board(3) + board(4) + board(5)) != 0)
        makeMoveAI(1)

      checkIfZeroOnField(1)

      checkIfEnd(0)

      if hasEnded then
        var ai1Pts: Int = board(13)
        var ai2Pts: Int = board(0) + board(1) + board(2) + board(3) + board(4) + board(5) + board(6)
        println("\nPoints of the AI1: " + ai1Pts)
        println("Points of AI2: " + ai2Pts)

        if ai2Pts > ai1Pts then
          println("AI2 has won!")
        else if ai2Pts < ai1Pts then
          println("AI1 has won!")
        else
          println("Draw!")

        return

      makeMoveAI(0)

      println("Last field: " + lastField)

      while(lastField == 13 && (board(7) + board(8) + board(9) + board(10) + board(11) + board(12)) != 0)
        makeMoveAI(0)

      checkIfZeroOnField(0)

      checkIfEnd(1)

      if hasEnded then
        var ai1Pts: Int = board(7) + board(8) + board(9) + board(10) + board(11) + board(12) + board(13)
        var ai2Pts: Int = board(6)
        println("\nPoints of AI1: " + ai1Pts)
        println("Points of AI2: " + ai2Pts)

        if ai2Pts > ai1Pts then
          println("AI2 has won!")
        else if ai2Pts < ai1Pts then
          println("AI1 has won!")
        else
          println("Draw!")

        return

  def playPlayerVsAI(): Unit =
    println("PLAYER VS AI MODE")

    print("Who will start the game [0 - AI, 1 - player]: ")

    var whoStarts: Int = scanner.nextInt()

    if whoStarts == 0 then makeMoveAI(0)

    if whoStarts != 0 && whoStarts != 1 then
      throw new Exception("Invalid option!")

    var playersChoice: Int = 0

    while(!hasEnded)
      getCurrentBoardStatus()

      print("\nFrom which field do you want to take stones " + playerName + ": ")

      playersChoice = scanner.nextInt()

      playersChoice = playersChoiceCheck(playersChoice)

      makeMove(playersChoice)

      while(lastField == 6 && (board(0) + board(1) + board(2) + board(3) + board(4) + board(5)) != 0)
        println("\nYour last stone has landed in your base! You get extra move!")
        print("From which field do you want to take stones " + playerName + ": ")
        playersChoice = scanner.nextInt()
        playersChoice = playersChoiceCheck(playersChoice)
        makeMove(playersChoice)

      checkIfZeroOnField(1)

      checkIfEnd(0)

      if hasEnded then
        var aiPts: Int = board(13)
        var playerPts: Int = board(0) + board(1) + board(2) + board(3) + board(4) + board(5) + board(6)
        println("\nPoints of the AI: " + aiPts)
        println("Points of " + playerName + ": " + playerPts)

        if playerPts > aiPts then
          println(playerName + " has won!")
        else if playerPts < aiPts then
          println("AI has won!")
        else
          println("Draw!")

        return

      makeMoveAI(0)

      while(lastField == 13 && (board(7) + board(8) + board(9) + board(10) + board(11) + board(12)) != 0)
        makeMoveAI(0)

      checkIfZeroOnField(0)

      for(i <- 0 to 13)
        simulatedBoard(i) = board(i)

      checkIfEnd(1)

      if hasEnded then
        var aiPts: Int = board(7) + board(8) + board(9) + board(10) + board(11) + board(12) + board(13)
        var playerPts: Int = board(6)
        println("\nPoints of the AI: " + aiPts)
        println("Points of " + playerName + ": " + playerPts)

        if playerPts > aiPts then
          println(playerName + " has won!")
        else if playerPts < aiPts then
          println("AI has won!")
        else
          println("Draw!")

        return


  def playersChoiceCheck(playersChoice: Int): Int =
    var playersChoiceTemp: Int = playersChoice

    while (playersChoiceTemp < 0 || playersChoiceTemp > 5 || board(playersChoiceTemp) == 0)
      println("You cannot take stones from this field! Try again: ")
      print("From which field do you want to take stones " + playerName + ": ")
      playersChoiceTemp = scanner.nextInt()

    playersChoiceTemp

  def checkIfZeroOnField(whichHalf: Int): Unit =
    if(whichHalf == 1)
      if board(lastField) - 1 == 0 && lastField >= 0 && lastField < 6 then
        println("\nYour last stone has landed in your empty field! You get opponent's stones!")
        board(6) += board(12 - lastField)
        board(12 - lastField) = 0
    if(whichHalf == 0)
      if board(lastField) - 1 == 0 && lastField >= 7 && lastField < 13 then
        board(13) += board(12 - lastField)
        board(12 - lastField) = 0

  def checkIfZeroOnFieldSimulation(whichHalf: Int, simulatedLastField: Int): Unit =
    if(whichHalf == 1)
      if simulatedBoard(simulatedLastField) - 1 == 0 && simulatedLastField >= 0 && simulatedLastField < 6 then
        simulatedBoard(6) += simulatedBoard(12 - simulatedLastField)
        simulatedBoard(12 - simulatedLastField) = 0
    if(whichHalf == 0)
      if simulatedBoard(simulatedLastField) - 1 == 0 && simulatedLastField >= 7 && simulatedLastField < 13 then
        simulatedBoard(13) += simulatedBoard(12 - simulatedLastField)
        simulatedBoard(12 - simulatedLastField) = 0

  def checkIfEnd(playerToCheck: Int): Unit =      //0 - CHECKS UPPER HALF; 1 - CHECKS LOWER HALF
    if playerToCheck == 0 then
      if board(7) == 0 && board(8) == 0 && board(9) == 0 && board(10) == 0 && board(11) == 0 && board(12) == 0 then
        println("AI cannot make a move! Let's count the points!")
        hasEnded = true

    if playerToCheck == 1 then
      if board(0) == 0 && board(1) == 0 && board(2) == 0 && board(3) == 0 && board(4) == 0 && board(5) == 0 then
        println("Player " + playerName + " cannot make a move! Let's count the points!")
        hasEnded = true
}
