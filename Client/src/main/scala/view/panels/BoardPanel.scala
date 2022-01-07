package view.panels

import controller.KalahaClientConnection

import java.awt.*
import javax.swing.border.LineBorder
import javax.swing.*

class BoardPanel extends JPanel:
    val firstPlayerHoles = Array.ofDim[JButton](6)
    val bases = Array.ofDim[JLabel](2)
    val secondPlayerHoles = Array.ofDim[JButton](6)

    val firstPlayerHolesPanel: JPanel = new JPanel()
    val basesPanel: JPanel = new JPanel
    val secondPlayerHolesPanel: JPanel = new JPanel

    val buttonDimension = new Dimension(60, 60)

    for (i <- firstPlayerHoles.indices)
        firstPlayerHoles(i) = new JButton("6")
        firstPlayerHoles(i).setPreferredSize(buttonDimension)
        firstPlayerHoles(i).setBackground(new Color(0xF09287))
        firstPlayerHolesPanel.add(firstPlayerHoles(i))

    bases(0) = new JLabel("0", SwingConstants.CENTER)
    bases(0).setPreferredSize(new Dimension(150, 30))
    bases(0).setBackground(new Color(0xF09287))
    bases(0).setBorder(new LineBorder(Color.RED, 3))
    bases(1) = new JLabel("0", SwingConstants.CENTER)
    bases(1).setPreferredSize(new Dimension(150, 30))
    bases(1).setBackground(new Color(0xF09287))
    bases(1).setBorder(new LineBorder(Color.RED, 3))
    basesPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 100, 0))
    basesPanel.add(bases(0))
    basesPanel.add(bases(1))
    basesPanel.setBorder(new LineBorder(Color.YELLOW, 3))


    for (i <- secondPlayerHoles.indices)
        secondPlayerHoles(i) = new JButton("6")
        secondPlayerHoles(i).setPreferredSize(buttonDimension)
        secondPlayerHoles(i).setBackground(new Color(0xF09287))
        secondPlayerHolesPanel.add(secondPlayerHoles(i))

    add(firstPlayerHolesPanel)
    add(basesPanel)
    add(secondPlayerHolesPanel)
    setBackground(new Color(0x99BBFF))
    setPreferredSize(new Dimension(510, 240))
    setBorder(new LineBorder(Color.BLUE, 3))
