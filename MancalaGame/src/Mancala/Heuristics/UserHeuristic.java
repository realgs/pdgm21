package Mancala.Heuristics;

import Mancala.Board.MancalaBoard;
import MyScanner.MyScanner;

public class UserHeuristic extends MancalaHeuristic {
    @Override
    public int selectMove(MancalaBoard board, int depth) {
        int player = board.currentPlayer();
        int bins = board.getBins();
        int bin = MyScanner.readInt(1, bins, "Please select a bin to move for Player" + player);
        while (board.getBin(bin) == 0) {
            bin = MyScanner.readInt(1, bins, "Please select a bin to move for Player" + player);
        }
        return bin;
    }

    @Override
    public int getHeuristicValue(MancalaBoard board) {
        return 0;
    }
}
