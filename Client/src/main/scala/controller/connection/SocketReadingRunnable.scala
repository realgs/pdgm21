package controller.connection

import controller.KalahaController

import java.io.BufferedReader

class SocketReadingRunnable(reader: BufferedReader, controller: KalahaController) extends Runnable:
    override def run(): Unit =
        try
            while true do
                val line = reader.readLine()
                line match
                    case "connected" => controller.onConnected()
                    case "registered" => controller.onRegistered()
                    case s"players: $firstPlayerName, $secondPlayerName" => controller.onPlayers(firstPlayerName, secondPlayerName)
                    case "game started" => controller.onGameStarted()
                    case s"remaining $remaining" => controller.onRemaining(remaining)
                    case s"$name time is up" => controller.onTimeIsUp(name)
                    case s"$name made move: $holeNumber" => controller.onMoveMade(name, holeNumber.toInt)
                    case "moveAllowed" => controller.onMoveAllowed()
                    case s"game over $winner" => controller.onGameOver(winner)
                    case s => println(s)
        catch
            case e: java.net.SocketException => println("Server closed connection!")