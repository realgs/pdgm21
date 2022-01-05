package controller

import model.KalahaPlayer
import service.KalahaService

class KalahaController:

    val service = new KalahaService

    def makeMove(playerName: String, holeNumber: Int): Unit =
        if !service.gameStarted then throw new IllegalStateException("Game has not started yet!")
        if playerName != service.turn then throw new IllegalArgumentException("This player: " + playerName + " cannot make move, it's " + service.turn + " turn")
        try {
            val player = service.getPlayerByUsername(playerName)
            val opponent = service.getOpponentByUsername(playerName)
            val lastStoneInBase = service.makeMove(player, holeNumber, opponent)
            if !lastStoneInBase then
                service.turn = (if service.turn == service.firstPlayer.name then service.secondPlayer.name else service.firstPlayer.name)
        } catch {
            case e: IllegalArgumentException => throw new IllegalArgumentException(playerName + " not found!")
        }
        ()

    def registerPlayer(name: String) =
        service.registerPlayer(name)

    def startGame =
        service.startGame

    def getPlayerNames =
        (service.firstPlayer.name, service.secondPlayer.name)

    def getPlayerByUsername(name: String) =
        service.getPlayerByUsername(name)

    def getOpponentByUsername(name: String) =
        service.getOpponentByUsername(name)