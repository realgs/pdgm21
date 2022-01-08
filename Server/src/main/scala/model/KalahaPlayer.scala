package model

class KalahaPlayer(playerName: String):
    val name = playerName
    val stonesInHoles: Array[Int] = Array(6, 6, 6, 6, 6, 6)
    var score: Int = 0

    def copy(): KalahaPlayer =
        val clonePlayer = new KalahaPlayer(name)
        clonePlayer.score = score
        for (i <- 0 until 6)
            clonePlayer.stonesInHoles(i) = stonesInHoles(i)
        clonePlayer
