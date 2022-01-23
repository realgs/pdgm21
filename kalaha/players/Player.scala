package players

abstract class Player(val id: Int, val name: String = ""):

    def makeMove(): Int
