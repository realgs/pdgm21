package client

import akka.actor.*

import scala.io.StdIn

class Human(id: Int) extends Player(id) {

  override def choosePit(): Int =

    var additionalValue = 0;
    println("Player " + id + " please enter pit index from " + 0 + " to " + 5)

    if (id == 2)
      additionalValue = 7

    StdIn.readInt() + additionalValue

}
