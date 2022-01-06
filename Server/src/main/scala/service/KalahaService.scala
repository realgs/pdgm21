package service

import model.{KalahaAI, KalahaPlayer}

class KalahaService:
    var playersRegistered = 0
    var firstPlayer: KalahaPlayer = new KalahaAI("AliceAI")
    var secondPlayer: KalahaPlayer = new KalahaAI("JohnnyAI")

    var gameStarted: Boolean = false
    var turn: String = ""
    var makeMoveDeadline: Long = 0

    def makeMove(player: KalahaPlayer, holeNumber: Int, opponent: KalahaPlayer): Boolean =
        var pl = player
        var num = holeNumber
        var stonesToMove = pl.stonesInHoles(num)
        pl.stonesInHoles(num) = 0
        while (stonesToMove > 0)
            num += 1
            if num == 6 then
                num = -1
                if pl == player then
                    pl.score += 1
                    stonesToMove -= 1
                pl = (if pl == player then opponent else player)
            else
                pl.stonesInHoles(num) += 1
                stonesToMove -= 1
        if num == -1 then true
        else
            if pl.stonesInHoles(num) == 1 && pl == player then
                pl.score += (if pl == player then opponent else player).stonesInHoles(5-num)
                (if pl == player then opponent else player).stonesInHoles(5-num) = 0
                pl.score += 1
                pl.stonesInHoles(num) = 0
            false

    def updatePredictions(ai: KalahaAI) =
        for (i <- 0 until 6)
            val aiCopy = ai.copy
            val opponentCopy = (if ai == firstPlayer then secondPlayer else firstPlayer).copy
            makeMove(aiCopy, i, opponentCopy)
            ai.predictions(i) = aiCopy.score

    def printBoard =
        for (i <- (1 until 6).reverse)
            print(s"${secondPlayer.stonesInHoles(i)} | ")
        println(secondPlayer.stonesInHoles(0))
        println(s"${secondPlayer.score}                     ${firstPlayer.score}")
        for (i <- 0 until 5)
            print(s"${firstPlayer.stonesInHoles(i)} | ")
        println(firstPlayer.stonesInHoles(5))

    def boardToString =
        var s = ""
        for (i <- (1 until 6).reverse)
            s += s"${secondPlayer.stonesInHoles(i)} | "
        s += s"${secondPlayer.stonesInHoles(0)}\n"
        s += s"${secondPlayer.score}                     ${firstPlayer.score}\n"
        for (i <- 0 until 5)
            s += s"${firstPlayer.stonesInHoles(i)} | "
        s += s"${firstPlayer.stonesInHoles(5)}\n"
        s
    
    def checkGameOver =
        val stones1 = firstPlayer.stonesInHoles
        val stones2 = secondPlayer.stonesInHoles
        (stones1(0) == 0 && stones1(1) == 0 && stones1(2) == 0 && stones1(3) == 0 && stones1(4) == 0 && stones1(5) == 0) ||
          (stones2(0) == 0 && stones2(1) == 0 && stones2(2) == 0 && stones2(3) == 0 && stones2(4) == 0 && stones2(5) == 0)

    def registerPlayer(name: String) =
        if name == firstPlayer.name || name == secondPlayer.name then throw new IllegalArgumentException("Given name is already in use!")
        if playersRegistered == 0 then
            firstPlayer = new KalahaPlayer(name)
            playersRegistered += 1
        else if playersRegistered == 1 then
            secondPlayer = new KalahaPlayer(name)
            playersRegistered += 1
        else throw new IllegalStateException("All players joined!")

    def getPlayerByUsername(name: String) =
        if firstPlayer.name == name then firstPlayer
        else if secondPlayer.name == name then secondPlayer
        else throw new IllegalArgumentException("No such user!")

    def getOpponentByUsername(name: String) =
        if firstPlayer.name == name then secondPlayer
        else if secondPlayer.name == name then firstPlayer
        else throw new IllegalArgumentException("No such opponent!")

    def startGame =
        if gameStarted then throw IllegalStateException("Game started before!")
        gameStarted = true
        turn = firstPlayer.name
        makeMoveDeadline = System.currentTimeMillis() + 30*1000