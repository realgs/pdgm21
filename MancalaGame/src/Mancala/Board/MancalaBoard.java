package Mancala.Board;

import Algorithms.Minimax.MinimaxProblem;
import Mancala.Heuristics.MancalaHeuristic;

import java.util.ArrayList;
import java.util.Arrays;

public class MancalaBoard implements MinimaxProblem, Cloneable {

    private int[][] players;
    private final int bins;
    private final int totalStones;
    private int currentPlayer;
    private int maxPlayer;
    private final MancalaHeuristic[] heuristics;
    private int stonesMoved = 0;
    private final int depth;

    private static final int STORAGE = 0;

    public MancalaBoard(int bins, int stonesPerBin, MancalaHeuristic h0, MancalaHeuristic h1, int depth) {
        this.bins = bins;
        this.totalStones = bins * stonesPerBin * 2;
        players = new int[2][bins + 1];
        for (int i = 0; i <= 1; i++) {
            for (int j = 1; j <= bins; j++) {
                players[i][j] = stonesPerBin;
            }
        }
        currentPlayer = 0;
        maxPlayer = 0;
        heuristics = new MancalaHeuristic[2];
        heuristics[0] = h0;
        heuristics[1] = h1;
        this.depth = depth;
    }

////*==================================== GETTER / SETTERS =============================*/

    public int getBins() {
        return bins;
    }

    public int getPlayersTotalStones(int playerNo) {
        int sum = 0;
        for (int j = 1; j <= bins; j++) {
            sum += players[playerNo][j];
        }
        return sum;
    }

    public int getStonesInStorage(int player) {
        return players[player][STORAGE];
    }

    private int getFirstNonEmptyBin(int player) {
        for (int i = 1; i <= bins; i++) {
            if (players[player][i] > 0) return i;
        }
        return -1;
    }

    public int stonesMoved() {
        return stonesMoved;
    }

    public int currentPlayer() {
        return currentPlayer;
    }

    public int getMaxPlayer() {
        return maxPlayer;
    }

    private int opponentPlayer() {
        return otherPlayer(currentPlayer);
    }

    public static int otherPlayer(int player) {
        return (player + 1) % 2;
    }

    public int getBin(int bin) {
        return players[currentPlayer][bin];
    }

    public int getBin(int player, int bin) {
        return players[player][bin];
    }

////*==================================== STATUS METHODS ===============================*/

    public boolean isGameOver() {
        return getPlayersTotalStones(currentPlayer) == 0 && getPlayersTotalStones(opponentPlayer()) == 0;
    }

    private boolean cannotMove() {
        for (int i = 1; i <= bins; i++) {
            if (players[currentPlayer][i] > 0) {
                return false;
            }
        }
        return true;
    }

////*====================================  ACTIONS   ===================================*/

    private void stealBin(int player, int bin) {
        int oppositeBin = bins + 1 - bin;
        int oppositePlayer = otherPlayer(player);
        players[player][STORAGE] += players[oppositePlayer][oppositeBin] + 1;
        players[oppositePlayer][oppositeBin] = 0;
    }

    private void flushStones(int player) {
        for (int i = 1; i <= bins; i++) {
            flushBin(player, i);
        }
    }


    private void flushBin(int player, int bin) {
        stonesMoved = players[player][bin];
        players[player][STORAGE] += stonesMoved;
        players[player][bin] = 0;
    }

    private void setCurrentPlayer(int currentPlayer) {
        this.currentPlayer = currentPlayer;
    }

    private void setMaxPlayer(int maxPlayer) {
        this.maxPlayer = maxPlayer;
    }

    public int move() {
        int bin = getFirstNonEmptyBin(currentPlayer());
        if (getPlayersTotalStones(opponentPlayer()) != 0) {
            this.setMaxPlayer(currentPlayer);
            bin = heuristics[currentPlayer].selectMove(this, depth);
        }
        move(bin);
        return bin;
    }


    public void move(int bin) {
        int stones = players[currentPlayer][bin];
        if (getPlayersTotalStones(opponentPlayer()) == 0) {
            flushStones(currentPlayer);
        }
        else {
            stonesMoved = stones;
            players[currentPlayer][bin] = 0;
            int currentSide = currentPlayer;
            int currentBin = bin - 1; // Start distributing stones in well to right of selected bin.
            for (int stonesLeft = stones; stonesLeft > 0; stonesLeft--) {
                if (
                        (stonesLeft == 1) && (currentSide == currentPlayer) && (currentBin > 0)
                        && (players[currentSide][currentBin] == 0)
                ) {
                    stealBin(currentPlayer, currentBin);
                }
                else if (currentBin == 0) {
                    if (currentSide == currentPlayer) {
                        players[currentSide][currentBin]++;
                        if (stonesLeft == 1) {
                            if (cannotMove()) {
                                flushStones(otherPlayer(currentPlayer));
                            }
                            return;
                        }
                    }
                    else {
                        stonesLeft++;
                    }
                    currentSide = otherPlayer(currentSide);
                    currentBin = bins;
                } else {
                    players[currentSide][currentBin]++;
                    currentBin--;
                }
            }
            setCurrentPlayer(opponentPlayer());
            if (cannotMove()) {
                flushStones(otherPlayer(currentPlayer));
            }
        }
    }

////*===============================  Minimax Methods   ================================*/

    @Override
    public double heuristicValue() {
        return heuristics[maxPlayer].getHeuristicValue(this);
    }

    public MancalaBoard getSuccessor(int bin) throws CloneNotSupportedException {
        MancalaBoard suc = (MancalaBoard) this.clone();
        suc.move(bin);
        return suc;
    }

    @Override
    public ArrayList<MinimaxProblem> children() {
        ArrayList<MinimaxProblem> children = new ArrayList<>();
        for (int i = 1; i <= bins; ++i) {
            try {
                if (players[currentPlayer][i] > 0)
                    children.add(i - 1, getSuccessor(i));
                else
                    children.add(i - 1, null);
            } catch (CloneNotSupportedException e) {
                e.printStackTrace();
            }
        }
        return children;
    }

    @Override
    public boolean isLeaf() {
        return isGameOver();
    }

    @Override
    public boolean isMaximizing() {
        return currentPlayer == maxPlayer;
    }

    @Override
    public boolean equals(MinimaxProblem o) {
        if (this == o) return true;
        if (o == null) return false;
        MancalaBoard mancalaBoard = (MancalaBoard) o;
        return bins == mancalaBoard.bins &&
                totalStones == mancalaBoard.totalStones &&
                currentPlayer == mancalaBoard.currentPlayer &&
                stonesMoved == mancalaBoard.stonesMoved &&
                Arrays.deepEquals(players, mancalaBoard.players);
    }

///*====================================================================================*/

    @Override
    protected Object clone() throws CloneNotSupportedException {
        MancalaBoard clone = (MancalaBoard) super.clone();
        clone.players = new int[this.players.length][this.players[0].length];
        for (int r = 0; r < this.players.length; r++)
            if (this.players[r].length >= 0)
                System.arraycopy(this.players[r], 0, clone.players[r], 0, this.players[r].length);
        return clone;
    }


///*======================== Console Print Utilities ===================================*/

    public String toString() {
        return edgeLine() + player0Line() + middleLine() + player1Line() + edgeLine() + " ";
    }

    public String edgeLine() {
        return "+----" + middleDashes() + "----+\n";
    }

    public String player0Line() {
        StringBuilder sb = new StringBuilder();
        sb.append("|    |");
        for (int i = 1; i <= bins; i++) {
            sb.append(" ").append(numberString(getBin(0, i))).append(" |");
        }
        sb.append("    |\n");
        return sb.toString();
    }

    public String middleLine() {
        return "| " + numberString(getBin(0, 0)) + " "
                + middleDashes()
                + " " + numberString(getBin(1, 0)) + " |\n";
    }

    public String player1Line() {
        StringBuilder sb = new StringBuilder();
        sb.append("|    |");
        for (int i = bins; i > 0; i--) {
            sb.append(" ").append(numberString(getBin(1, i))).append(" |");
        }
        sb.append("    |\n");
        return sb.toString();
    }

    public String middleDashes() {
        StringBuilder sb = new StringBuilder();
        for (int i = 1; i <= bins; i++) {
            sb.append("+----");
        }
        sb.append("+");
        return sb.toString();
    }

    public String numberString(int n) {
        // Return a two character string with an integer.
        // Assumes input is in range [0..99].
        if ((0 <= n) && (n < 10)) {
            return " " + n;
        } else {
            return Integer.toString(n);
        }
    }

    public static void printBoardConfiguration() {
        MancalaBoard board = new MancalaBoard(6, 1, null, null, 0);
        for (int p = 0; p < 2; p++) {
            for (int bin = 0; bin <= 6; ++bin) {
                board.players[p][bin] = bin;
            }
        }
        System.out.println("Board index Configuration: ");
        System.out.println(" ----- Player 0 ----- ");
        System.out.println(board + " ----- Player 1 ----- " + "\n\n");
    }
///*====================================================================================*/


}

