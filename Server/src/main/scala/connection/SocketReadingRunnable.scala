package connection

import controller.KalahaController

import java.io.{BufferedReader, InputStreamReader}
import java.net.Socket

class SocketReadingRunnable(socket: Socket, controller: KalahaController) extends Runnable:
    val port = socket.getPort()
    val reader: BufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream()))
    override def run(): Unit =
        try
            while true do
                val line = reader.readLine()
                if line == null then socket.close()
                line match
                    case "disconnect" => controller.timerThread.interrupt()
                    case s"joinGame$name" => controller.onJoinGame(port, name)
                    case s"makeMove $name; $hole" => controller.broadcastMakeMove(port, name, hole.toInt)
                    case "animationDone" => controller.onAnimationDone(port)
                    case "showPlayers" => controller.onShowPlayers(port)
                    case "startGame" => controller.onStartGame(port)
                    case s => println("WS: other: " + s)
        catch
            case e: java.net.SocketException => println("Client closed connection!")
