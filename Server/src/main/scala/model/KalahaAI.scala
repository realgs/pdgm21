package model

class KalahaAI(name: String) extends KalahaPlayer(name):

    val predictions: Array[Int] = Array.ofDim(6)

    override def copy: KalahaAI =
        val clonePlayer = new KalahaAI(name)
        clonePlayer.score = score
        for (i <- 0 until 6)
            clonePlayer.stonesInHoles(i) = stonesInHoles(i)
            clonePlayer.predictions(i) = predictions(i)
        clonePlayer

    def printPredisctions =
        for (i <- 0 until 5)
            print(s"${predictions(i)} | ")
        println(predictions(5))

    def selectBestMove: Int =
        var bestMove = 0
        var max = 0
        for (i <- 0 until 6)
            if (predictions(i) > max) then
                bestMove = i
                max = predictions(i)
        bestMove