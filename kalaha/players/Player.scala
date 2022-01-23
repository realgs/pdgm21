package players

import players.PlayerID.PlayerID

abstract class Player(val id: PlayerID, name: String = ""):

    def makeMove(): Int
