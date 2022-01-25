package kalaha.service

import kalaha.player.{KalahaAI, KalahaPlayer}

class Service:
    var status: String = "not started"     // not started, waiting for move, animating, finished
    var turn: String = ""
    var turnSwitched: Boolean = false
    var makeMoveDeadline: Long = 0

    var playersRegistered: Int = 0
    var firstPlayer: KalahaPlayer = new KalahaAI("AliceAI")
    var secondPlayer: KalahaPlayer = new KalahaAI("JohnnyAI")
    
    def makeMove(playerName: String, holeNumber: Int): Unit =
        if status != "waiting for move" then throw new IllegalStateException("Game has not started yet!")
        if playerName != turn then throw new IllegalArgumentException("This player: " + playerName + " cannot make move, it's " + turn + " turn")
        try
            status = "animating"
            val player = getPlayerByUsername(playerName)
            val opponent = getOpponentByUsername(playerName)
            val lastStoneInBase = player.affectBoard(holeNumber, opponent)
            if !lastStoneInBase then
                turnSwitched = true
                turn = (if turn == firstPlayer.name then secondPlayer.name else firstPlayer.name)
            else turnSwitched = false
        catch
            case e: IllegalArgumentException => throw new IllegalArgumentException(playerName + " not found!")
    
    def updatePredictions(ai: KalahaAI): Unit =
        for i <- ai.predictions.indices do
            val aiCopy = ai.copy
            val opponentCopy = (if ai == firstPlayer then secondPlayer else firstPlayer).copy
            aiCopy.affectBoard(i, opponentCopy)
            ai.predictions(i) = aiCopy.score

    def printBoard: Unit =
        for i <- (1 until 6).reverse do
            print(s"${secondPlayer.stonesInHoles(i)} | ")
        println(secondPlayer.stonesInHoles(0))
        println(s"${secondPlayer.score}                     ${firstPlayer.score}")
        for i <- 0 until 5 do
            print(s"${firstPlayer.stonesInHoles(i)} | ")
        println(firstPlayer.stonesInHoles(5))
    
    def checkGameOver: Boolean =
        val stones1 = firstPlayer.stonesInHoles
        val stones2 = secondPlayer.stonesInHoles
        (stones1(0) == 0 && stones1(1) == 0 && stones1(2) == 0 && stones1(3) == 0 && stones1(4) == 0 && stones1(5) == 0) ||
          (stones2(0) == 0 && stones2(1) == 0 && stones2(2) == 0 && stones2(3) == 0 && stones2(4) == 0 && stones2(5) == 0)
        
    def getWinner: String =
        for stones <- firstPlayer.stonesInHoles do firstPlayer.score += stones
        for stones <- secondPlayer.stonesInHoles do secondPlayer.score += stones
        if firstPlayer.score > secondPlayer.score then firstPlayer.name
        else if firstPlayer.score < secondPlayer.score then secondPlayer.name
        else "draw"

    def registerPlayer(name: String): Unit =
        if name == firstPlayer.name || name == secondPlayer.name then throw new IllegalArgumentException("Given name is already in use!")
        if playersRegistered == 0 then
            firstPlayer = new KalahaPlayer(name)
            playersRegistered += 1
        else if playersRegistered == 1 then
            secondPlayer = new KalahaPlayer(name)
            playersRegistered += 1
        else throw new IllegalStateException("All players joined!")

    def getPlayerByUsername(name: String): KalahaPlayer =
        if firstPlayer.name == name then firstPlayer
        else if secondPlayer.name == name then secondPlayer
        else throw new IllegalArgumentException("No such user!")

    def getOpponentByUsername(name: String): KalahaPlayer =
        if firstPlayer.name == name then secondPlayer
        else if secondPlayer.name == name then firstPlayer
        else throw new IllegalArgumentException("No such opponent!")

    def startGame: Unit =
        if status != "not started" then throw IllegalStateException("Game started before!")
        status = "waiting for move"
        turn = firstPlayer.name
        makeMoveDeadline = System.currentTimeMillis() + 30*1000
        
    def onAnimationDone: Unit =
        status = "waiting for move"
        makeMoveDeadline = System.currentTimeMillis() + 30*1000
        
    def onTimeoutDefeat: Unit =
        status = "finished"
        firstPlayer = new KalahaAI("AliceAI")
        secondPlayer = new KalahaAI("JohnnyAI")