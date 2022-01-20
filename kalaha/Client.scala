package kalaha

import scala.concurrent.{Await, Future, TimeoutException}
import concurrent.ExecutionContext.Implicits.global
import concurrent.duration.DurationInt

class Client {
  val server: Server = new Server
  var currentPlayer = 0

  def displayPreGameMenu(): Unit =
    println("--- Kalaha ---")
    println("1. Singleplayer")
    println("2. Multiplayer")
    println("3. Simulation")
    println("4. Exit")
    print("choice: > ")

    val choice = scala.io.StdIn.readInt()
    choice match
      case 1 => runSingleplayer()
      case 2 => runMultiplayer()
      case 3 => runSimulation()
      case 4 => println("Bye")
      case _ => println("Invalid input, please try again.")
                displayPreGameMenu()

  def displayGameMenu(): Unit =
    if !server.gameStarted then displayPreGameMenu()
    else {
      while (!server.isOver) {
        println(server.getBoard)
        currentPlayer = server.getCurrentPlayer
        if (currentPlayer == 1) then
          print("\nPlayer 1 turn. Choose hole (1-6): > ")
        else
          print("\nPlayer 2 turn. Choose hole (8-13): > ")
        var choice = -1
        var response = -1
        try {
          val makeMoveRequest = Future(scala.io.StdIn.readInt())
          choice = Await.result(makeMoveRequest, 30.seconds)
          response = server.requestMove(choice)
        } catch {
          case e: TimeoutException => printf("\n\nPlayer %d loses due to inactivity\n", currentPlayer)
                                      printf("Score %d : %d%n", server.getScore._1, server.getScore._2)
                                      System.exit(0);
        }
        response match
          case -2 => println("Game is already finished")
          case -1 => println("Invalid input, please try again")
          case -3 => println("Game error")
          case  1 => println("You have one more move")
          case  _ => println("")
        displayGameMenu()
      }
      displayWinner()
    }

  def displaySingleplayerMenu(bot: AIPlayer): Unit =
    if !server.gameStarted then displayPreGameMenu()
    else {
      while (!server.isOver) {
        println(server.getBoard)
        print("\nYour turn. Choose hole (1-6): > ")
        var choice = -1
        var response = -1
        try {
          val makeMoveRequest = Future(scala.io.StdIn.readInt())
          choice = Await.result(makeMoveRequest, 30.seconds)
          response = server.requestMove(choice)
        } catch {
          case e: TimeoutException => println("\n\nYou lose due to inactivity")
                                      printf("Score %d : %d%n", server.getScore._1, server.getScore._2)
                                      System.exit(0);
        }

        response match
          case -2 => println("Game is already finished")
          case -1 => println("Invalid input, please try again")
          case -3 => println("Game error")
          case  1 => println("You have one more move")
          case  _ => println("")

        if response == 0 then
          println(server.getBoard)
          println(s"\nComputer's turn, hole selected: ${bot.makeMove().mkString(", ")}\n")

        displaySingleplayerMenu(bot)
      }
      displayWinner()
    }

  def runSingleplayer(): Unit =
    val bot = new AIPlayer(7, 13, server)
    server.setPlayers(new Player, bot)
    server.startGame()
    displaySingleplayerMenu(bot)

  def runMultiplayer(): Unit =
    server.setPlayers(new Player, new Player)
    server.startGame()
    displayGameMenu()

  def runSimulation(): Unit =
    val bot1 = new AIPlayer(0, 6, server)
    val bot2 = new AIPlayer(7, 13, server)
    server.setPlayers(bot1, bot2)
    server.startGame()
    runSimulation(bot1, bot2)

  def runSimulation(bot1: AIPlayer, bot2: AIPlayer): Unit =
    while (!server.isOver) {
      println(server.getBoard)
      currentPlayer = server.getCurrentPlayer
      if (currentPlayer == 1) then
        println(s"\nComputer 1 turn, hole selected: ${bot1.makeMove().mkString(", ")}\n")
      else
        println(s"\nComputer 2 turn, hole selected: ${bot2.makeMove().mkString(", ")}\n")
      runSimulation(bot1, bot2)
    }
    displayWinner()

  def displayWinner(): Unit =
    server.collectStonesToBase()
    val winner = server.getWinner
    winner match
      case 0 => println("Draw!")
      case 1 => println("Player 1 won!")
      case 2 => println("Player 2 won!")
    printf("Score %d : %d%n", server.getScore._1, server.getScore._2)
    System.exit(0)
}
