package controller

import connection.ClientConnection
import model.{KalahaAI, KalahaPlayer}
import service.{KalahaService, TimerRunnable}

import java.util
import java.util.HashMap
import java.util.Map.Entry

class KalahaController(connections: HashMap[Int, ClientConnection]):
    val service: KalahaService = new KalahaService()
    val timerThread: Thread = new Thread(new TimerRunnable(this))
    timerThread.start()

    def getSecondPort(port: Int): Int =
        val connList: util.ArrayList[Entry[Int, ClientConnection]] = new util.ArrayList[Entry[Int, ClientConnection]](connections.entrySet())
        var foundPort: Int = -1
        for (i <- 0 until connList.size())
            if connList.get(i).getKey() != port then foundPort = connList.get(i).getKey()
        foundPort

    def getPortByName(name: String): Int =
        val connList: util.ArrayList[Entry[Int, ClientConnection]] = new util.ArrayList[Entry[Int, ClientConnection]](connections.entrySet())
        var foundPort: Int = -1
        for (i <- 0 until connList.size())
            if connList.get(i).getValue().name == name then foundPort = connList.get(i).getKey()
        foundPort

    def onJoinGame(port: Int, name: String): Unit =
        try
            service.registerPlayer(name)
            connections.get(port).commandRegistered(name)
        catch
            case e => connections.get(port).commandError(e.getMessage())

    def broadcastMakeMove(port: Int, name: String, holeNumber: Int): Unit =
        try
            makeMove(port, name, holeNumber)
            service.printBoard()
            val connList: util.ArrayList[Entry[Int, ClientConnection]] = new util.ArrayList[Entry[Int, ClientConnection]](connections.entrySet())
            for (i <- 0 until connList.size())
                connList.get(i).getValue().commandMadeMove(name, holeNumber)
            if service.checkGameOver() then
                timerThread.interrupt()
                val winner = service.getWinner()
                connections.get(port).commandGameOver(winner)
        catch
            case e => connections.get(port).commandError(e.getMessage())

    def onShowPlayers(port: Int): Unit =
        connections.get(port).commandPlayers(service.firstPlayer.name, service.secondPlayer.name)

    def onStartGame(port: Int): Unit =
        try
            service.startGame()
            connections.get(port).commandGameStarted()
            if service.turn.endsWith("AI") then
                var ai = service.getPlayerByUsername(service.turn).asInstanceOf[KalahaAI]
                service.updatePredictions(ai)
                val aiBestMove = ai.selectBestMove()
                broadcastMakeMove(port, service.turn, aiBestMove)
            else
                connections.get(getPortByName(service.turn)).commandMoveAllowed()
        catch
            case e => connections.get(port).commandError(e.getMessage)

    def makeMove(port: Int, playerName: String, holeNumber: Int): Unit =
        if service.status != "waiting for move" then throw new IllegalStateException("Game has not started yet!")
        if playerName != service.turn then throw new IllegalArgumentException("This player: " + playerName + " cannot make move, it's " + service.turn + " turn")
        try
            service.status = "animating"
            val player = service.getPlayerByUsername(playerName)
            val opponent = service.getOpponentByUsername(playerName)
            val lastStoneInBase = service.makeMove(player, holeNumber, opponent)
            if !lastStoneInBase then
                service.turnSwitched = true
                service.turn = (if service.turn == service.firstPlayer.name then service.secondPlayer.name else service.firstPlayer.name)
            else service.turnSwitched = false
        catch
            case e: IllegalArgumentException => throw new IllegalArgumentException(playerName + " not found!")

    def onAnimationDone(port: Int): Unit =
        service.onAnimationDone()
        if service.turn.endsWith("AI") then
            Thread.sleep(3000)
            var ai: KalahaAI = service.getPlayerByUsername(service.turn).asInstanceOf[KalahaAI]
            service.updatePredictions(ai)
            val aiBestMove = ai.selectBestMove()
            broadcastMakeMove(port, service.turn, aiBestMove)
        else
            service.checkGameOver()
            var thisTurnPlayerPort = getPortByName(service.turn)
            connections.get(thisTurnPlayerPort).commandMoveAllowed()

    def broadcastTimeoutDefeat(): Unit =
        timerThread.interrupt()
        service.onTimeoutDefeat()
        val connList: util.ArrayList[Entry[Int, ClientConnection]] = new util.ArrayList[Entry[Int, ClientConnection]](connections.entrySet())
        for (i <- 0 until connList.size())
            connList.get(i).getValue().commandTimeIsUp(service.turn)

    def broadcastRemaining(remaining: Long): Unit =
        val connList: util.ArrayList[Entry[Int, ClientConnection]] = new util.ArrayList[Entry[Int, ClientConnection]](connections.entrySet())
        for (i <- 0 until connList.size())
            connList.get(i).getValue().commandRemaining(remaining)