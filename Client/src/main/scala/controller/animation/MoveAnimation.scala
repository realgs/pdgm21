package controller.animation

import view.panels.BoardPanel

import java.awt.{Color, Toolkit}
import javax.swing.{JButton, JLabel}

class MoveAnimation(boardPanel: BoardPanel):
    val firstPlayerHoles: Array[JButton] = boardPanel.firstPlayerHoles
    val bases: Array[JLabel] = boardPanel.bases
    val secondPlayerHoles: Array[JButton] = boardPanel.secondPlayerHoles

    def animate(movingPlayer: Int, holeNumber: Int): Unit =
        val movingPlayerHoles: Array[JButton] = (if movingPlayer == 0 then firstPlayerHoles else secondPlayerHoles)
        var currentPlayerHoles: Array[JButton] = movingPlayerHoles
        val currentPlayerBase: JLabel = bases(movingPlayer)
        var stonesToDistribute = currentPlayerHoles(holeNumber).getText().toInt
        val distributionSourceHole: JButton = currentPlayerHoles(holeNumber)
        distributionSourceHole.setBackground(Color.RED)
        var num = holeNumber

        while stonesToDistribute > 0 do
            Thread.sleep(250)
            num += 1
            if num == 6 then
                num = -1
                if currentPlayerHoles sameElements movingPlayerHoles then
                    currentPlayerBase.setText(s"${currentPlayerBase.getText().toInt + 1}")
                    stonesToDistribute -= 1
                    distributionSourceHole.setText(s"${distributionSourceHole.getText().toInt - 1}")
                currentPlayerHoles = (if currentPlayerHoles sameElements firstPlayerHoles then secondPlayerHoles else firstPlayerHoles)
            else
                currentPlayerHoles(num).setText(s"${currentPlayerHoles(num).getText().toInt + 1}")
                stonesToDistribute -= 1
                distributionSourceHole.setText(s"${distributionSourceHole.getText().toInt - 1}")

        if num != -1 && currentPlayerHoles(num).getText().toInt == 1 && (currentPlayerHoles sameElements movingPlayerHoles) then
            currentPlayerBase.setText(s"${currentPlayerBase.getText().toInt + (if movingPlayer == 0 then secondPlayerHoles else firstPlayerHoles)(5-num).getText().toInt}")
            (if movingPlayer == 0 then secondPlayerHoles else firstPlayerHoles)(5-num).setText("0")
            currentPlayerBase.setText(s"${currentPlayerBase.getText().toInt + 1}")
            currentPlayerHoles(num).setText("0")

        distributionSourceHole.setBackground(Color.BLACK)