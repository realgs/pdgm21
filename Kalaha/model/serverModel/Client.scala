package model.serverModel

import model.GameSpecification
import model.playerModel.HumanPlayer
import model.serverModel.Server
import views.MainView

import java.io.*
import java.net.{ServerSocket, Socket}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class Client(_socket: Socket, _username: String):

  private val socket: Socket = _socket
  private val dataInput: DataInputStream = new DataInputStream(socket.getInputStream)
  private val dataOutput: DataOutputStream = new DataOutputStream(socket.getOutputStream)
  private val playerUsername: String = _username
  private var nameOfOtherPlayer: String = ""
  private var player: HumanPlayer = null
  private var isYourTurn: Boolean = true
  private var enemiesScore: Int = -1
  private var timeState: String = PlayerHandler.TIMEGOOD

  private def getInput(): Int =
    print(": ")
    try
      scala.io.StdIn.readInt()
    catch
      case e: NumberFormatException =>
        if !socket.isClosed then
          getInput()
        else -1

  private def draw(): Unit =
    MainView.drawSpace()
    print(s"$nameOfOtherPlayer")
    println()
    print("\t\t")
    for index <- 1 to GameSpecification.INDEXFIRSTSTORE do
      print(s"${player.getBoard()(GameSpecification.INDEXSECONDSTORE - index)} \t")

    println()
    print(s"\t${player.getBoard()(GameSpecification.INDEXSECONDSTORE)}")

    for index <- 0 to GameSpecification.INDEXFIRSTSTORE do print("\t")

    print(player.score)
    println()
    print("\t\t")


    for index <- 0 until GameSpecification.INDEXFIRSTSTORE do
      print(s"${player.arrayOfHomes(index)} \t")

    println()
    print(s"${player.name}\t\t")
    println("\n")

  def checkIfStarting(): Unit =
    if socket.isConnected then
      dataOutput.writeUTF(playerUsername)
      dataOutput.flush()
      val newPlayerName = dataInput.readUTF()
      val isStarting = dataInput.read()
      val amountOfRocks = dataInput.read()
      GameSpecification.resetAmountOfRocks(amountOfRocks)
      player = new HumanPlayer(newName = newPlayerName)
      player.changeIsStarting(isStarting == 1)
      isYourTurn = isStarting == 1

  def waitingRoom(): Unit =
    if socket.isConnected then
      MainView.drawSpace()
      println("Waiting for another player!")
      while !socket.isClosed && nameOfOtherPlayer == "" do
        Thread.sleep(100)
        if dataInput.available() > 0 then
          timeState = dataInput.readUTF()
          nameOfOtherPlayer = dataInput.readUTF()

      if timeState == PlayerHandler.TIMEOUT then
        close(true)
      else
        listen()
        makeMove()


  def makeMove(): Unit =
    while !socket.isClosed do
      try
        if isYourTurn then
          var chosenIndex = 0
          val availableIndexes = player.getAvailableMoves()

          chosenIndex = getInput()
          if !socket.isClosed then
            while !availableIndexes.contains(chosenIndex - 1) do
              chosenIndex = getInput()

            val (moveIndex, isEnding, nextMove, _) = player.makeMove(chosenIndex - 1)
            isYourTurn = nextMove
            val newScore = player.countEndingScore()
            enemiesScore = GameSpecification.STARTAMOUNTOFROCKS * 12 - newScore
            dataOutput.write(moveIndex)
            dataOutput.write(if isEnding then 1 else 0)
            dataOutput.write(if nextMove then 1 else 0)
            dataOutput.write(newScore)
            dataOutput.flush()


            draw()
            Thread.sleep(500)
            if isEnding then close()


      catch
        case e:IOException => close()


  def listen(): Unit =
    draw()
    Future {
      var timeState: String = ""
      var moveIndex: Int = 0
      var isEnding: Int = 0
      var isNextMove: Int = 0
      while moveIndex != -1  && isEnding != -1 && isNextMove != -1 && isEnding != 1 do
        Thread.sleep(100)
        if dataInput.available() > 0 then
          timeState = dataInput.readUTF()
          moveIndex = dataInput.read()
          isEnding = dataInput.read()
          isNextMove = dataInput.read()
          if timeState != PlayerHandler.TIMEOUT then
            enemiesScore = dataInput.read()
            player.updateAfterEnemiesMove(moveIndex)
            draw()
          else
            dataInput.read()

          if isEnding == 1 then
            close()
          isYourTurn = isNextMove == 0

    }

  def close(isOnlyOnePlayer: Boolean = false): Unit =
    if !isOnlyOnePlayer then
      player.score = player.countEndingScore()
      print(s"\nYour points: ${player.score}, Enemies: ${if enemiesScore == -1 then GameSpecification.STARTAMOUNTOFROCKS * 12 - player.score else enemiesScore}")
    else
      print(s"\nWaiting time expire")
    dataInput.close()
    dataOutput.close()
    socket.close()





