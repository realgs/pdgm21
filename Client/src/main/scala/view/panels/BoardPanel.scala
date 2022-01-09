package view.panels

import controller.connection.KalahaClientConnection

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

    private val buttonDimension: Dimension = new Dimension(60, 60)
    private val font: Font = new Font("Arial Narrow", Font.PLAIN, 24)

    for (i <- firstPlayerHoles.indices)
        firstPlayerHoles(i) = new JButton("6")
        firstPlayerHoles(i).setPreferredSize(buttonDimension)
        firstPlayerHoles(i).setBackground(Color.BLACK)
        firstPlayerHoles(i).setFont(font)
        firstPlayerHoles(i).setForeground(Color.WHITE)
        firstPlayerHoles(i).setEnabled(false)
        firstPlayerHolesPanel.add(firstPlayerHoles(i))

    for (i <- bases.indices.reverse)
        bases(i) = new JLabel("0", SwingConstants.CENTER)
        bases(i).setPreferredSize(new Dimension(150, 50))
        bases(i).setBorder(new LineBorder(Color.BLACK, 3))
        bases(i).setFont(font)
        basesPanel.add(bases(i))
    basesPanel.setLayout(new FlowLayout(FlowLayout.CENTER, 100, 0))


    for (i <- secondPlayerHoles.indices.reverse)
        secondPlayerHoles(i) = new JButton("6")
        secondPlayerHoles(i).setPreferredSize(buttonDimension)
        secondPlayerHoles(i).setBackground(Color.BLACK)
        secondPlayerHoles(i).setFont(font)
        secondPlayerHoles(i).setForeground(Color.WHITE)
        secondPlayerHoles(i).setEnabled(false)
        secondPlayerHolesPanel.add(secondPlayerHoles(i))

    add(secondPlayerHolesPanel)
    add(basesPanel)
    add(firstPlayerHolesPanel)
    setPreferredSize(new Dimension(510, 240))
