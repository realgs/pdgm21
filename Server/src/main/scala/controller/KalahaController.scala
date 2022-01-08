package controller

import model.{KalahaAI, KalahaPlayer}
import service.{KalahaService, TimerRunnable}
import cask.*
import status.GameStatus

class KalahaController(channel: WsChannelActor):
    val service = new KalahaService
    var timerThread: Thread = null
    val gameStatus: GameStatus = new GameStatus()

    def onConnect(): Unit =
        channel.send(Ws.Text("connected"))

    def onJoinGame(name: String): Unit =
        try
            service.registerPlayer(name)
            channel.send(Ws.Text("registered"))
        catch
            case e => channel.send(Ws.Text(e.getMessage))

    def onMakeMove(name: String, holeNumber: Int): Unit =
        try
            gameStatus.status = "animating"
            makeMove(name, holeNumber)
            service.printBoard()
            channel.send(Ws.Text(name + " made move: " + holeNumber))
            if service.checkGameOver() then
                channel.send(Ws.Text("Game over!"))
                val winner = service.getWinner()
                if winner == "draw" then channel.send(Ws.Text("Result: draw"))
                else channel.send(Ws.Text(s"$winner wins!"))
        catch
            case e => channel.send(Ws.Text(e.getMessage))

    def onShowPlayers(): Unit =
        channel.send(Ws.Text(s"players: ${service.firstPlayer.name}, ${service.secondPlayer.name}"))

    def onStartGame(): Unit =
        try
            service.startGame()
            gameStatus.status = "waiting for move"
            channel.send(Ws.Text("game started"))
            if service.turn.endsWith("AI") then
                var ai = service.getPlayerByUsername(service.turn).asInstanceOf[KalahaAI]
                service.updatePredictions(ai)
                val aiBestMove = ai.selectBestMove()
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
        try
            val player = service.getPlayerByUsername(playerName)
            val opponent = service.getOpponentByUsername(playerName)
            val lastStoneInBase = service.makeMove(player, holeNumber, opponent)
            if !lastStoneInBase then
                service.turn = (if service.turn == service.firstPlayer.name then service.secondPlayer.name else service.firstPlayer.name)
        catch
            case e: IllegalArgumentException => throw new IllegalArgumentException(playerName + " not found!")

    def onAnimationDone(): Unit =
        gameStatus.status = "waiting for move"
        service.makeMoveDeadline = System.currentTimeMillis() + 30*1000
        println("Setting new deadline: " + service.makeMoveDeadline)
        if service.turn.endsWith("AI") then
            Thread.sleep(3000)
            var ai: KalahaAI = service.getPlayerByUsername(service.turn).asInstanceOf[KalahaAI]
            service.updatePredictions(ai)
            val aiBestMove = ai.selectBestMove()
            println(s"${service.turn} making move, number = $aiBestMove")
            onMakeMove(service.turn, aiBestMove)

    def onTimeoutDefeat(): Unit =
        service.gameStarted = false
        service.firstPlayer = new KalahaAI("AliceAI")
        service.secondPlayer = new KalahaAI("JohnnyAI")
        channel.send(Ws.Text(s"${service.turn} time is up"))