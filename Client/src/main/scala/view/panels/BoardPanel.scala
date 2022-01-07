package view.panels

import controller.KalahaClientConnection

import java.awt.*
import javax.swing.border.LineBorder
import javax.swing.*

class BoardPanel extends JPanel:
    val firstPlayerHoles: Array[JButton] = Array.ofDim[JButton](6)
    val bases: Array[JLabel] = Array.ofDim[JLabel](2)
    val secondPlayerHoles: Array[JButton] = Array.ofDim[JButton](6)

    val firstPlayerHolesPanel: JPanel = new JPanel()
    val basesPanel: JPanel = new JPanel
    val secondPlayerHolesPanel: JPanel = new JPanel

    private val buttonDimension = new Dimension(60, 60)

    for (i <- firstPlayerHoles.indices)
        firstPlayerHoles(i) = new JButton("6")
        firstPlayerHoles(i).setPreferredSize(buttonDimension)
        firstPlayerHoles(i).setBackground(new Color(0xF09287))
        firstPlayerHoles(i).setEnabled(false)
        firstPlayerHolesPanel.add(firstPlayerHoles(i))

    for (i <- bases.indices)
        bases(i) = new JLabel("0", SwingConstants.CENTER)
        bases(i).setPreferredSize(new Dimension(150, 30))
        bases(i).setBackground(new Color(0xF09287))
        bases(i).setBorder(new LineBorder(Color.RED, 3))
        basesPanel.add(bases(i))
    basesPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 100, 0))
    basesPanel.setBorder(new LineBorder(Color.YELLOW, 3))


    for (i <- secondPlayerHoles.indices)
        secondPlayerHoles(i) = new JButton("6")
        secondPlayerHoles(i).setPreferredSize(buttonDimension)
        secondPlayerHoles(i).setBackground(new Color(0xF09287))
        secondPlayerHoles(i).setEnabled(false)
        secondPlayerHolesPanel.add(secondPlayerHoles(i))

    add(firstPlayerHolesPanel)
    add(basesPanel)
    add(secondPlayerHolesPanel)
    setBackground(new Color(0x99BBFF))
    setPreferredSize(new Dimension(510, 240))
    setBorder(new LineBorder(Color.BLUE, 3))
