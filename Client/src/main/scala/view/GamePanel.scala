package view

import java.awt.{Color, FlowLayout}
import javax.swing.border.LineBorder
import javax.swing.{JButton, JLabel, JPanel}

class GamePanel extends JPanel:
    var startGameButton: JButton = new JButton("Start game")
    var timeoutLabel: JLabel = new JLabel("Time remaining: 30 s")
    
    add(startGameButton)
    add(timeoutLabel)
    setLayout(new FlowLayout(FlowLayout.CENTER, 10, 10))
    setBorder(new LineBorder(Color.GREEN, 3))