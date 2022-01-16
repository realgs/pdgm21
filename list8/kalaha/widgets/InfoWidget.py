from qtpy.QtWidgets import QWidget, QHBoxLayout, QLabel, QLCDNumber, QSizePolicy, QSpacerItem
from qtpy.QtCore import Slot, Qt


class InfoWidget(QWidget):
    def __init__(self, parent):
        super(InfoWidget, self).__init__(parent)
        font = self.font()
        font.setPointSize(11)

        self.widget_layout = QHBoxLayout(self)
        self.setLayout(self.widget_layout)
        self.widget_layout.setContentsMargins(25, 11, 25, 25)

        self.lcd_widget = QLCDNumber(self)
        self.lcd_widget.setDigitCount(2)
        self.lcd_widget.display(0)
        self.widget_layout.addWidget(self.lcd_widget)

        self.turn_label = QLabel(self)
        self.turn_label.setFont(font)
        self.turn_label.setText("Waiting for opponent...")
        self.turn_label.setAlignment(Qt.AlignCenter)
        self.widget_layout.addWidget(self.turn_label)

        self.spacer_right = QSpacerItem(100, 20, QSizePolicy.Expanding, QSizePolicy.Minimum)
        self.widget_layout.addItem(self.spacer_right)

        self.player_number_label = QLabel(self)
        self.player_number_label.setFont(font)
        self.player_number_label.setText("Player ...")
        self.player_number_label.setAlignment(Qt.AlignCenter)
        self.widget_layout.addWidget(self.player_number_label)

        self.auto_play_options_label = QLabel(self)
        self.auto_play_options_label.setFont(font)
        self.auto_play_options_label.setAlignment(Qt.AlignCenter)
        self.widget_layout.addWidget(self.auto_play_options_label)
        self.auto_play_options_label.hide()

    @Slot(bool, int, int, bool, bool)
    def set_auto_play_options(self, auto_play, minimax_depth, auto_play_delay,
                              no_alpha_beta, iterative_deepening):
        self.auto_play_options_label.setText(f"AP: {int(auto_play)} "
                                             f"MD: {minimax_depth} "
                                             f"APD: {auto_play_delay} "
                                             f"AB: {int(not no_alpha_beta)} "
                                             f"ID: {int(iterative_deepening)}")
        self.auto_play_options_label.show()

    @Slot(bool)
    def set_auto_play_options_visible(self, value: bool):
        self.auto_play_options_label.setVisible(value)

    @Slot(int)
    def set_player_number(self, player_number: int):
        self.player_number_label.setText(f"Player {player_number}")

    @Slot(str)
    def set_player_turn(self, message: str):
        self.turn_label.setText(message)

    @Slot(int)
    def set_timeout(self, time: int):
        self.lcd_widget.display(time)
