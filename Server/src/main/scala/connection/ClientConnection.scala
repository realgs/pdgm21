package connection

import controller.KalahaController

import java.io.PrintWriter
import java.net.Socket

class ClientConnection(socket: Socket, controller: KalahaController):
    val writer: PrintWriter = new PrintWriter(socket.getOutputStream, true)
    var name: String = ""
    new Thread(new SocketReadingRunnable(socket, controller)).start()
    commandConnected()

    def commandConnected(): Unit =
        writer.println("connected")

    def commandRegistered(name: String): Unit =
        this.name = name
        writer.println("registered")

    def commandError(message: String): Unit =
        writer.println(s"error: $message")

    def commandMadeMove(name: String, holeNumber: Int): Unit =
        writer.println(s"$name made move: $holeNumber")

    def commandGameOver(winner: String): Unit =
        writer.println(s"game over $winner")

    def commandPlayers(name1: String, name2: String): Unit =
        writer.println(s"players: $name1, $name2")

    def commandGameStarted(): Unit =
        writer.println("game started")

    def commandTimeIsUp(name: String): Unit =
        writer.println(s"$name time is up")

    def commandRemaining(remaining: Long): Unit =
        writer.println(s"remaining $remaining")
        
    def commandMoveAllowed(): Unit =
        writer.println("moveAllowed")