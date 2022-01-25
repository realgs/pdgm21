package kalaha.controller

import kalaha.connection.ClientConnection
import kalaha.player.KalahaAI
import kalaha.service.{Service, TimerRunnable}

import java.util
import java.util.HashMap
import java.util.Map.Entry
import scala.collection.mutable.ArrayBuffer

class Controller(connections: ArrayBuffer[ClientConnection]):
    val service: Service = new Service
    val timerThread: Thread = new Thread(new TimerRunnable(this))
    timerThread.start()
        
    def getConnectionByName(name: String): ClientConnection =
        var foundConnection: ClientConnection = null
        for c <- connections do
            if c.name == name then foundConnection = c
        foundConnection

    def onJoinGame(name: String): Unit =
        try
            service.registerPlayer(name)
            getConnectionByName(name).commandRegistered(name)
        catch
            case e => getConnectionByName(name).commandError(e.getMessage)

    def broadcastMakeMove(name: String, holeNumber: Int): Unit =
        try
            service.makeMove(name, holeNumber)
            for c <- connections do
                c.commandMadeMove(name, holeNumber)
            if service.checkGameOver then
                timerThread.interrupt()
                val winner = service.getWinner
                getConnectionByName(name).commandGameOver(winner)
        catch
            case e => getConnectionByName(name).commandError(e.getMessage)

    def onShowPlayers(name: String): Unit =
        getConnectionByName(name).commandPlayers(service.firstPlayer.name, service.secondPlayer.name)

    def onStartGame(name: String): Unit =
        try
            service.startGame
            getConnectionByName(name).commandGameStarted
            if service.turn.endsWith("AI") then
                val ai = service.getPlayerByUsername(service.turn).asInstanceOf[KalahaAI]
                service.updatePredictions(ai)
                val aiBestMove = ai.selectBestMove
                broadcastMakeMove(service.turn, aiBestMove)
            else
                getConnectionByName(service.turn).commandMoveAllowed
        catch
            case e => getConnectionByName(name).commandError(e.getMessage)

    def onAnimationDone: Unit =
        service.onAnimationDone
        if service.turn.endsWith("AI") then
            Thread.sleep(3000)
            var ai: KalahaAI = service.getPlayerByUsername(service.turn).asInstanceOf[KalahaAI]
            service.updatePredictions(ai)
            val aiBestMove = ai.selectBestMove
            broadcastMakeMove(service.turn, aiBestMove)
        else
            service.checkGameOver
            getConnectionByName(service.turn).commandMoveAllowed

    def broadcastTimeoutDefeat: Unit =
        timerThread.interrupt()
        service.onTimeoutDefeat
        for c <- connections do
            c.commandTimeIsUp(service.turn)

    def broadcastRemaining(remaining: Long): Unit =
        for c <- connections do
            c.commandRemaining(remaining)