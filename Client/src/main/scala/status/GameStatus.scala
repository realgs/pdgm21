package status

class GameStatus:
    var name: String = ""
    var playerNames: Array[String] = Array.ofDim[String](2)
    playerNames(0) = "AliceAI"
    playerNames(1) = "JohnnyAI"