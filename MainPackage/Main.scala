package com.example.MainPackage

import com.example.GamePackage.Game
import com.example.KalahaGUI.Gui
import com.example.MainPackage.Test.test

object Main extends App {
  val gameGui = new Gui
  gameGui.buildGameLayout()

  //val game = new Game(Game.createBoardArray(6), Game.getFirstPlayer())
  //val (score1, score2) = test(game)
  //System.out.println((score1, score2) == (43, 29))
}
