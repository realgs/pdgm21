import controller.{ClientRunnable, KalahaController}
import service.{KalahaService, TimerRunnable}

import java.net.ServerSocket
import java.net.Socket
import controller.ClientRunnable

object Main:
    var controller: KalahaController = null
    var serverSocket: ServerSocket = new ServerSocket(6666)

    def main(args: Array[String]): Unit =
        while true do
            var client: Socket = serverSocket.accept()
            new Thread(new ClientRunnable(client)).start()