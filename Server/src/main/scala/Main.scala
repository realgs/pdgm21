import connection.{ClientConnection, SocketReadingRunnable}
import controller.KalahaController
import service.{KalahaService, TimerRunnable}

import java.net.ServerSocket
import java.net.Socket
import java.util.HashMap

object Main:
    val connections: HashMap[Int, ClientConnection] = new HashMap[Int, ClientConnection]()
    val controller: KalahaController = new KalahaController(connections)
    val serverSocket: ServerSocket = new ServerSocket(6666)

    def main(args: Array[String]): Unit =
        while true do
            var client: Socket = serverSocket.accept()
            println("Port = " + client.getPort())
            connections.put(client.getPort(), new ClientConnection(client, controller))
