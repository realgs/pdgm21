package view

import java.awt.GridLayout
import javax.swing.{JButton, JPanel, JTextPane}

class BoardPanel extends JPanel:
    val firstPlayerHoles = Array.ofDim[JButton](6)
    val bases = Array.ofDim[JTextPane](2)
    val secondPlayerHoles = Array.ofDim[JButton](6)
    
    val firstPlayerHolesPanel: JPanel = new JPanel()
    val basesPanel = new JPanel
    val secondPlayerHolesPanel = new JPanel
  
    for (i <- firstPlayerHoles.indices)
        firstPlayerHoles(i) = new JButton("6")
        firstPlayerHolesPanel.add(firstPlayerHoles(i))
    basesPanel.setLayout(new GridLayout(1, 2, 100, 10))
  
    for (i <- bases.indices)
        bases(i) = new JTextPane()
        bases(i).setText("0")
        basesPanel.add(bases(i))
  
    secondPlayerHolesPanel.setLayout(new GridLayout(1, 6, 10, 10))
    for (i <- secondPlayerHoles.indices)
      secondPlayerHoles(i) = new JButton("6")
      secondPlayerHolesPanel.add(secondPlayerHoles(i))
  
    add(secondPlayerHolesPanel)
    add(basesPanel)
    add(firstPlayerHolesPanel)
    setLayout(new GridLayout(3, 1, 10, 10))
