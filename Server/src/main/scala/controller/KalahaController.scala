package controller

import model.{KalahaAI, KalahaPlayer}
import service.{KalahaService, TimerRunnable}
import status.GameStatus

import java.io.PrintWriter

class KalahaController(writer: PrintWriter):
    val service = new KalahaService()
    var timerThread: Thread = null
    val gameStatus: GameStatus = new GameStatus()

    def onConnect(): Unit =
        println("onConnect")
        writer.println("connected\n")

    def onJoinGame(name: String): Unit =
        try
            service.registerPlayer(name)
            writer.println("registered")
        catch
            case e => writer.println(e.getMessage)

    def onMakeMove(name: String, holeNumber: Int): Unit =
        try
            gameStatus.status = "animating"
            makeMove(name, holeNumber)
            service.printBoard()
            writer.println(s"$name made move: $holeNumber")
            if service.checkGameOver() then
                writer.println("Game over!")
                val winner = service.getWinner()
                if winner == "draw" then writer.println("Result: draw")
                else writer.println(s"$winner wins!")
        catch
            case e => writer.println(e.getMessage)

    def onShowPlayers(): Unit =
        writer.println(s"players: ${service.firstPlayer.name}, ${service.secondPlayer.name}")

    def onStartGame(): Unit =
        try
            service.startGame()
            gameStatus.status = "waiting for move"
            writer.println("game started")
            if service.turn.endsWith("AI") then
                var ai = service.getPlayerByUsername(service.turn).asInstanceOf[KalahaAI]
                service.updatePredictions(ai)
                val aiBestMove = ai.selectBestMove()
                println(s"${service.turn} making move, number = $aiBestMove")
                onMakeMove(service.turn, aiBestMove)
            else
                timerThread = new Thread(new TimerRunnable(writer, this))
                timerThread.start
        catch
            case e: IllegalStateException => writer.println(e.getMessage)

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
        writer.println(s"${service.turn} time is up")