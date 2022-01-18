package Server

import java.io.{BufferedReader, BufferedWriter, InputStreamReader, OutputStreamWriter}
import java.net.Socket
import java.io.IOException
import Game.Game.*
import Player.Player.*
import _root_.Player.Computer.*
import _root_.Player.Human.*

import java.util.concurrent.TimeoutException
import scala.concurrent.*
import scala.concurrent.duration.Duration
import ExecutionContext.Implicits.global

object ClientHandler {

  private val clientHandlers: Array[ClientHandler] = new Array(2)
  private val game = new Game()

  def readyToPlay(): Boolean = clientHandlers(0).player != null && clientHandlers(1).player != null

  def clientsConnected(): Boolean = clientHandlers(0) != null && clientHandlers(1) != null

  def gamePlay(): Unit = {
    var endOfGame = false
    var caught = false

    while (clientHandlers(PLAYER_1).socket.isConnected() && clientHandlers(PLAYER_2).socket.isConnected() && !caught && !endOfGame) {
      if readyToPlay() then {
        try {
          val activeID = game.whoseTurn
          clientHandlers(activeID).active = true
          println(s"\n${clientHandlers(activeID).clientUsername}'s turn")

          var wait = true //waiting for player's move
          while (wait) {
            Thread.sleep(100)
            if !clientHandlers(activeID).active then wait = false
          }
          Thread.sleep(100)
          endOfGame = game.isEnd()
          if (endOfGame) {
            clientHandlers(activeID).broadcastMessage(game.printResult(game.getWinner()), PLAYER_1)
            clientHandlers(activeID).broadcastMessage(game.printResult(game.getWinner()), PLAYER_2)
          }

          game.whoseTurn = clientHandlers(activeID).opponentID
        } catch case _: IOException =>
          clientHandlers(PLAYER_1).closeEverything(clientHandlers(PLAYER_1).socket, clientHandlers(PLAYER_1).br, clientHandlers(PLAYER_1).bw)
          clientHandlers(PLAYER_2).closeEverything(clientHandlers(PLAYER_2).socket, clientHandlers(PLAYER_2).br, clientHandlers(PLAYER_2).bw)
          caught = true
      } else Thread.sleep(1000)
    }
  }

  class ClientHandler(var socket: Socket) {
    var br: BufferedReader = _
    var bw: BufferedWriter = _
    var clientUsername: String = _

    var player: Player = _
    var ID: Int = _
    var opponentID: Int = _
    var active: Boolean = _

    try {
      bw = new BufferedWriter(new OutputStreamWriter(socket.getOutputStream()))
      br = new BufferedReader(new InputStreamReader(socket.getInputStream()))
      clientUsername = br.readLine()

      if clientHandlers(PLAYER_1) == null then
        clientHandlers(PLAYER_1) = this
        ID = PLAYER_1
        opponentID = PLAYER_2
      else {
        clientHandlers(PLAYER_2) = this
        ID = PLAYER_2
        opponentID = PLAYER_1
      }
    } catch case _: IOException => closeEverything(socket, br, bw)

    def handleClients(): Unit = {
      chooseMode()
      var caught = false
      while (socket.isConnected() && !caught) {
        if active then {
          try {
            var move = true
            broadcastMessage(game.printBoard(), ID)
            while (move) {
              val hole = getHole()
              move = player.makeMove(hole)
              println(clientUsername + ": " + hole)
              broadcastMessage(game.printBoard(), ID)
              broadcastMessage(clientUsername + "'s move: " + hole, opponentID)
            }
          } catch {
            case _: IOException => closeEverything(socket, br, bw)
              caught = true
          } finally active = false
        } else {
          Thread.sleep(1000)
        }
      }
    }

    def getHole(): Int = {
      clearBufferReader()
      broadcastMessage(s"You are Player ${ID + 1} \nYour turn: ", ID)

      var hole = -1
      player match {
        case computer: Computer =>
          Thread.sleep(500)
          computer.AI match
            case true => hole = computer.findHole()
            case false => hole = computer.randomHole()
          broadcastMessage("Your move: " + hole, ID)
        case _ =>
          var incorrect = true
          broadcastMessage("Enter a number of hole: ", ID)
          var timer = 300
          while (incorrect && timer > 0) {
            try {
              if br.ready() then {
                hole = Integer.parseInt(br.readLine())
                if game.checkInput(hole) then incorrect = false
                else broadcastMessage(s"Incorrect hole number. Please, try again. ${timer / 10} seconds left...", ID)
              }
            } catch case _: Exception => broadcastMessage("That's impossible. Please, try again: ", ID)
            finally
              timer -= 1
              Thread.sleep(100)
          }
          if hole < 0 then
            hole = Computer(game, false,0).randomHole()
            broadcastMessage(s"The end of your turn. A random move was made: hole: $hole", ID)
      }
      hole
    }

    def chooseMode(): Unit = {
      var correct = false
      while (!correct) {
        broadcastMessage(game.printPlayerOptions(), ID)
        val input = br.readLine()
        input match
          case "1" => player = new Human(game)
            correct = true
          case "2" => player = new Computer(game, false, 0)
            correct = true
          case "3" => player = new Computer(game, true, 1)
            correct = true
          case "4" => player = new Computer(game, true, 5)
            correct = true
          case _ => broadcastMessage("Wrong mode:" + input, ID)
      }
    }

    def broadcastMessage(messageToSend: String, destID: Int): Unit = {
      try {
        clientHandlers(destID).bw.write(messageToSend)
        clientHandlers(destID).bw.newLine()
        clientHandlers(destID).bw.flush()
      } catch case _: IOException => closeEverything(socket, br, bw)
    }

    def clearBufferReader(): Unit = {
      while (br.ready()) {
        br.readLine()
      }
    }

    def removeClientHandler(): Unit = {
      if clientHandlers(0) == this then clientHandlers(0) = null
      else clientHandlers(1) = null
      broadcastMessage("SERVER: " + clientUsername + " has left!", opponentID)
    }

    def closeEverything(socket: Socket, bufferedReader: BufferedReader, bufferedWriter: BufferedWriter): Unit = {
      removeClientHandler()
      try {
        if bufferedReader != null then bufferedReader.close()
        if bufferedWriter != null then bufferedWriter.close()
        if socket != null then socket.close()
      } catch case e: IOException => e.printStackTrace()
    }
  }
}
