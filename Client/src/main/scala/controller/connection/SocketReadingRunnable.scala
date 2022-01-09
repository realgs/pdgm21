package controller.connection

import controller.KalahaController

import java.io.BufferedReader

class SocketReadingRunnable(reader: BufferedReader, controller: KalahaController) extends Runnable:
    override def run(): Unit =
        println("socket reading runnable started")
        while true do
            val line = reader.readLine()
            println(line)
            line match
                case "connected" => controller.onConnected()
                case "registered" => controller.onRegistered()
                case s"players: ${firstPlayerName}, ${secondPlayerName}" => controller.onPlayers(firstPlayerName, secondPlayerName)
                case "game started" => controller.onGameStarted()
                case s"remaining $remaining" => controller.onRemaining(remaining)
                case s"$name time is up" => controller.onTimeIsUp()
                case s"$name made move: $holeNumber" => controller.onMoveMade(name, holeNumber.toInt)
                case s => println(s)