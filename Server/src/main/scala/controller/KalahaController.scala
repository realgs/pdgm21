package controller

import model.{KalahaPlayer, KalahaAI}
import service.{KalahaService, TimerRunnable}
import cask.*

class KalahaController(channel: WsChannelActor):

    val service = new KalahaService
    var timerThread: Thread = null

    def onConnect(name: String) =
        try
            service.registerPlayer(name)
            channel.send(Ws.Text("Registered " + name + ", players registered: " + service.firstPlayer.name + ", " + service.secondPlayer.name))
        catch
            case e => channel.send(Ws.Text(e.getMessage))

    def onMakeMove(name: String, holeNumber: Int): Unit =
        try
            makeMove(name, holeNumber)
            service.printBoard
            channel.send(Ws.Text(name + " made his move\n" + service.boardToString))
            if service.turn == "JohnnyAI" then
                var ai = service.secondPlayer.asInstanceOf[KalahaAI]
                service.updatePredictions(ai)
                val aiBestMove = ai.selectBestMove
                println("JohnnyAI making move, number = " + aiBestMove)
                onMakeMove("JohnnyAI", aiBestMove)
        catch
            case e => channel.send(Ws.Text(e.getMessage))

    def onShowPlayers =
        channel.send(Ws.Text("Players registered: " + service.firstPlayer.name + ", " + service.secondPlayer.name))

    def onStartGame =
        try
            service.startGame
            channel.send(Ws.Text("Game started with players: " + service.firstPlayer.name + ", " + service.secondPlayer.name))
            timerThread = new Thread(new TimerRunnable(channel, this))
            timerThread.start
        catch
            case e: IllegalStateException => channel.send(Ws.Text(e.getMessage))

    def makeMove(playerName: String, holeNumber: Int): Unit =
        if !service.gameStarted then throw new IllegalStateException("Game has not started yet!")
        if playerName != service.turn then throw new IllegalArgumentException("This player: " + playerName + " cannot make move, it's " + service.turn + " turn")
        try {
            val player = service.getPlayerByUsername(playerName)
            val opponent = service.getOpponentByUsername(playerName)
            val lastStoneInBase = service.makeMove(player, holeNumber, opponent)
            if !lastStoneInBase then
                service.turn = (if service.turn == service.firstPlayer.name then service.secondPlayer.name else service.firstPlayer.name)
            service.makeMoveDeadline = System.currentTimeMillis + 30*1000
        } catch {
            case e: IllegalArgumentException => throw new IllegalArgumentException(playerName + " not found!")
        }
        ()

    def onMoveMade =
        channel.send(Ws.Text("Move is made, now it's " + service.turn + " turn"))

    def onTimeoutDefeat =
        service.gameStarted = false
        service.firstPlayer = new KalahaAI("AliceAI")
        service.secondPlayer = new KalahaAI("JohnnyAI")
        channel.send(Ws.Text(s"${service.turn}, you lose, time is up!"))