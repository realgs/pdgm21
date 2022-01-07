package controller

import model.{KalahaPlayer, KalahaAI}
import service.{KalahaService, TimerRunnable}
import cask.*

class KalahaController(channel: WsChannelActor):
    val service = new KalahaService
    var timerThread: Thread = null

    def onConnect =
        channel.send(Ws.Text("connected"))

    def onJoinGame(name: String) =
        try
            service.registerPlayer(name)
            channel.send(Ws.Text("registered"))
        catch
            case e => channel.send(Ws.Text(e.getMessage))

    def onMakeMove(name: String, holeNumber: Int): Unit =
        try
            makeMove(name, holeNumber)
            service.printBoard
            channel.send(Ws.Text(name + " made move: " + service.boardToWsFormat()))
            if service.checkGameOver then
                channel.send(Ws.Text("Game over!"))
                val winner = service.getWinner
                if winner == "draw" then channel.send(Ws.Text("Result: draw"))
                else channel.send(Ws.Text(s"$winner wins!"))
            else if service.turn.endsWith("AI") then
                var ai = service.getPlayerByUsername(service.turn).asInstanceOf[KalahaAI]
                service.updatePredictions(ai)
                val aiBestMove = ai.selectBestMove
                println(s"${service.turn} making move, number = $aiBestMove")
                onMakeMove(service.turn, aiBestMove)
        catch
            case e => channel.send(Ws.Text(e.getMessage))

    def onShowPlayers =
        channel.send(Ws.Text(s"players: ${service.firstPlayer.name}, ${service.secondPlayer.name}"))

    def onStartGame =
        try
            service.startGame
            channel.send(Ws.Text("game started"))
            if service.turn.endsWith("AI") then
                var ai = service.getPlayerByUsername(service.turn).asInstanceOf[KalahaAI]
                service.updatePredictions(ai)
                val aiBestMove = ai.selectBestMove
                println(s"${service.turn} making move, number = $aiBestMove")
                onMakeMove(service.turn, aiBestMove)
            else
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
        channel.send(Ws.Text(s"${service.turn} time is up"))