package view

import java.awt.GridLayout
import javax.swing.{JPanel, JTextPane}

class StatusPanel extends JPanel:
    val firstPlayerName: JTextPane = new JTextPane()
    firstPlayerName.setText("AliceAI")
    val secondPlayerName: JTextPane = new JTextPane()
    secondPlayerName.setText("JohnnyAI")
    val timeRemaining = new JTextPane()
    timeRemaining.setText("30s")
    add(firstPlayerName)
    add(secondPlayerName)
    add(timeRemaining)
    setLayout(new GridLayout(3, 1, 10, 10))