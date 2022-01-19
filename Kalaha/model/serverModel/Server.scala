package model.serverModel

import model.serverModel.PlayerHandler

import java.io.IOException
import java.net.{ServerSocket, Socket}
import java.util.concurrent.TimeoutException
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

class Server(port: Int = 4444):
  private val serverSocket = new ServerSocket(port)
  private var playerAmount: Int = 0

  def startServer(): Unit =
    try
      while !serverSocket.isClosed && PlayerHandler.time < PlayerHandler.maxTimeOut + 2 do
        Thread.sleep(100)

        if playerAmount < 2 then
          val acceptSocket = Future{serverSocket.accept()}

          try
            val socket = Await.result(acceptSocket, Duration(PlayerHandler.maxTimeOut, "seconds"))
            if playerAmount == 1 then
              PlayerHandler.isFirstPlayer = false

            val playerHandler = new PlayerHandler(socket, "Player" + playerAmount)
            println("A new player has connected")
            val thread = new Thread(playerHandler)
            thread.start()
            playerAmount += 1
          catch
            case e: TimeoutException => serverClose()



    catch
      case e: IOException => serverClose()

    println("Server is closed")
    serverClose()


  def serverClose(): Unit =
    try
        serverSocket.close()
    catch
      case e: IOException => e.printStackTrace()
