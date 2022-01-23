class Game:
  var player1 = List(1, 2, 3, 4, 5, 6)
  var player2 = List(1, 2, 3, 4, 5, 6)

  def printBoard(): Unit =
    for(i <- player1.reverse)
      print("|" + i)
    println("|")
    for(i <- player2)
      print("|" + i)
    println("|")
