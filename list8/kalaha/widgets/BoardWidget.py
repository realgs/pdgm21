from qtpy.QtWidgets import QWidget, QHBoxLayout, QVBoxLayout
from qtpy.QtCore import Signal, Slot

import logging

from kalaha.widgets import PitButton, BaseButton
from kalaha.models import Board
from kalaha.stylesheets import *


class BoardWidget(QWidget):
    pit_clicked = Signal(int)

    def __init__(self, parent):
        super(BoardWidget, self).__init__(parent)
        self.pits = []
        self.logger = logging.getLogger(self.__class__.__name__)

        self.player_one_base = None
        self.player_two_base = None

        self.widget_layout = QHBoxLayout(self)
        self.widget_layout.setContentsMargins(25, 25, 25, 0)
        self.setLayout(self.widget_layout)

        self.pits_widget = QWidget(self)
        self.pits_layout = QVBoxLayout(self.pits_widget)
        self.pits_layout.setContentsMargins(11, 0, 11, 0)
        self.pits_widget.setLayout(self.pits_layout)

        self.player_one_pits_widget = QWidget(self.pits_widget)
        self.player_one_pits_layout = QHBoxLayout(self.player_one_pits_widget)
        self.player_one_pits_layout.setContentsMargins(11, 11, 11, 0)
        self.player_one_pits_widget.setLayout(self.player_one_pits_layout)

        self.player_two_pits_widget = QWidget(self.pits_widget)
        self.player_two_pits_layout = QHBoxLayout(self.player_two_pits_widget)
        self.player_two_pits_layout.setContentsMargins(11, 0, 11, 11)
        self.player_two_pits_widget.setLayout(self.player_two_pits_layout)

        self.pits_layout.addWidget(self.player_two_pits_widget)
        self.pits_layout.addWidget(self.player_one_pits_widget)

    @Slot(int)
    def set_board_size(self, size: int):
        self.pits = []
        for i in range(size):
            pit = PitButton(i, self.player_one_pits_widget)
            self.pits.append(pit)
            self.player_one_pits_layout.addWidget(pit)

        self.player_one_base = BaseButton(size, self)
        self.pits.append(self.player_one_base)

        for i in range(size):
            pit = PitButton(size + i + 1, self.player_two_pits_widget)
            self.pits.append(pit)
            self.player_two_pits_layout.insertWidget(0, pit)

        self.player_two_base = BaseButton(size + size + 1, self)
        self.pits.append(self.player_two_base)

        self.widget_layout.addWidget(self.player_two_base)
        self.widget_layout.addWidget(self.pits_widget)
        self.widget_layout.addWidget(self.player_one_base)

    @Slot(Board, list)
    def setup_board(self, board, allowed_pits):
        self.set_board(board)
        for pit_index in allowed_pits:
            self.pits[pit_index].setStyleSheet(PLAYER_PIT_STYLESHEET)
        self.pits[max(allowed_pits) + 1].setStyleSheet(PLAYER_PIT_STYLESHEET)

    @Slot(Board)
    def set_board(self, board: Board):
        self.logger.debug(f"Setting board: {board.board}")
        if len(self.pits) != 0 and len(board.board) != len(self.pits):
            raise ValueError("Pit count does not match")
        else:
            self.set_board_size(board.board_size)
            self.update_pits(board)
            self.logger.debug(f"Done! Pits indexes are: {[pit.index for pit in self.pits]}")
            for pit in self.pits:
                pit.clicked.connect(self.on_pit_clicked)

    @Slot(Board)
    def update_pits(self, board: Board):
        for i in range(len(board.board)):
            self.pits[i].setText(str(board.board[i]))

    @Slot()
    def on_pit_clicked(self):
        pit: PitButton = self.sender()
        self.pit_clicked.emit(pit.index)
