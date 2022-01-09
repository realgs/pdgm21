package controller

import java.io.{BufferedInputStream, BufferedReader, InputStreamReader, PrintWriter}
import java.net.Socket

class ClientRunnable(socket: Socket) extends Runnable:
    override def run(): Unit =
        val reader: BufferedReader = new BufferedReader(new InputStreamReader(socket.getInputStream))
        val writer: PrintWriter = new PrintWriter(socket.getOutputStream, true)
        val controller: KalahaController = new KalahaController(writer)
        println("TCP: connect")
        controller.onConnect()
        while true do
            val line = reader.readLine()
            println("Line = " + line)
            line match
                case "disconnect" =>
                    println("WS: disconnect")
                    controller.timerThread.interrupt()
                case s"joinGame$name" =>
                    println("WS: joinGame" + name)
                    controller.onJoinGame(name)
                case s"makeMove $name; $hole" =>
                    println("WS: makeMove " + name + " " + hole)
                    controller.onMakeMove(name, hole.toInt)
                case "animationDone" =>
                    println("WS: animationDone")
                    controller.onAnimationDone()
                case "showPlayers" =>
                    println("WS: showPlayers")
                    controller.onShowPlayers()
                case "startGame" =>
                    println("WS: startGame")
                    controller.onStartGame()
                case s => println("WS: other: " + s)