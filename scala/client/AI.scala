package client

import akka.actor.*
import scala.util.Random

import scala.io.StdIn

class AI(id: Int) extends Player(id) {

  override def choosePit(): Int =

    println("Player " + id + " turn!")

    Thread.sleep(300)

    var rand = 0;

    if (id == 1)
      rand = Random.between(0, 6)
    else
      rand = Random.between(7, 13)

    while (!board.isChosenPitCorrect(rand)) {

      if (id == 1)
        rand = Random.between(0, 6)
      else
        rand = Random.between(7, 13)
    }
    rand
}
