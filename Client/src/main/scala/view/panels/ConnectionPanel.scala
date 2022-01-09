package view.panels

import controller.connection.KalahaClientConnection

import java.awt.{Color, FlowLayout}
import javax.swing.border.LineBorder
import javax.swing.{JButton, JLabel, JPanel, SwingConstants}

class ConnectionPanel extends JPanel:
    var connectButton: JButton = new JButton("Connect to server")
    var connectStatus: JLabel = new JLabel("Disconnected", SwingConstants.CENTER)

    add(connectButton)
    add(connectStatus)
    setLayout(new FlowLayout(FlowLayout.CENTER, 10, 10))