package Mancala.Heuristics;

import Mancala.Board.MancalaBoard;

public class Heuristic1 extends MancalaHeuristic {
    @Override
    public int getHeuristicValue(MancalaBoard board) {
        int maxPlayer = board.getMaxPlayer();
        int minPlayer = MancalaBoard.otherPlayer(maxPlayer);
        int stones_in_my_storage = board.getStonesInStorage(maxPlayer);
        int stones_in_opponents_storage = board.getStonesInStorage(minPlayer);
        return stones_in_my_storage - stones_in_opponents_storage;
    }
}
