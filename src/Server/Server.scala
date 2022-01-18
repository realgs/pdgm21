package Server

import java.net.ServerSocket
import java.net.Socket
import ClientHandler.*

import java.io.IOException
import scala.concurrent.*
import ExecutionContext.Implicits.global

object Server {

  class Server(var serverSocket: ServerSocket) {

    def startServer(): Unit = {
      try {
        var started = false
        while (!serverSocket.isClosed()) {
          val socket = serverSocket.accept()
          val clientHandler = new ClientHandler(socket)
          println(s"A new player ${clientHandler.clientUsername} has connected!")

          Future {clientHandler.handleClients()}
          
          if !started && clientsConnected() then {
            started = true
            println("The game started")
            gamePlay()
          }
        }
      } catch case _ : IOException => closeServerSocket()
    }

    def closeServerSocket(): Unit = {
      try if serverSocket != null then serverSocket.close()
      catch case e: IOException => e.printStackTrace()
    }

  }

  def main(args: Array[String]): Unit = {
    val serverSocket = new ServerSocket(1234)
    val server = new Server(serverSocket)
    server.startServer()
  }
}
