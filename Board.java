public class Board {
    private Pit[] board;

    public Board() {
        board = new Pit[14];
        int numberOnBoard = 0;
        for (int i = 0; i < board.length; i++) {
            if (i < 6) {
                board[i] = new Pit(6, 1, numberOnBoard++);
            } else if (i == 6) {
                board[i] = new Pit(0, 1, numberOnBoard++);
                numberOnBoard = 0;
            } else if (i < 13) {
                board[i] = new Pit(6, 2, numberOnBoard++);
            } else if (i == 13) {
                board[i] = new Pit(0, 2, numberOnBoard++);
            }
        }
    }
    public Board(Pit[] board){
        this.board=board;
    }

    public boolean move(int position, int whichPlayer) {
        if(whichPlayer==2){
            position=position+7;
        }
        if (checkPosition(position, whichPlayer)) {
            int stones = board[position].getStones();
            board[position].setStones(0);
            position++;

            while (stones > 0) {
                if (whichPlayer == 1) {
                    if (position != 13) {
                        board[position].setStones(board[position].getStones() + 1);
                        position = (position + 1) % 14;
                        stones = stones - 1;
                    } else position = 0;
                } else {
                    if (position != 6) {
                        board[position].setStones(board[position].getStones() + 1);
                        position = (position + 1) % 14;
                        stones = stones - 1;
                    } else position = 7;
                }
            }

            //when all moved
            int lastStonePosition;
            if (position == 0) {
                lastStonePosition = 13;
            } else lastStonePosition = position - 1;
            //additional move
            bonusMove(lastStonePosition, whichPlayer);
            return true;
        } else return false;
    }

    public boolean checkPosition(int position, int whichPlayer) {
        if (board[position].getStones() == 0) return false;
        else if (whichPlayer == 1) {
            if (position < 0 || position > 5) {
                return false;
            } else return true;
        } else {
            if (position < 7 || position > 12) {
                return false;
            } else return true;
        }
    }

    public void bonusMove(int lastStonePosition, int whichPlayer) {
        if ((whichPlayer == 1 && board[lastStonePosition].getStones() - 1 == 0 && lastStonePosition >= 0 && lastStonePosition < 6 ||
                whichPlayer == 2 && board[lastStonePosition].getStones() - 1 == 0 && lastStonePosition >= 7 && lastStonePosition < 13)) {
            int oppositePosition = 12 - lastStonePosition;
            board[lastStonePosition].setStones(board[lastStonePosition].getStones() + board[oppositePosition].getStones());
            board[oppositePosition].setStones(0);
        }
    }

    public boolean checkIfEnd() {
        int emptyPit1 = 0;
        int emptyPit2 = 0;
        for (int i = 0; i < 6; i++) {
            if (board[i].getStones() == 0) {
                emptyPit1++;
            }
        }
        for (int i = 7; i < 13; i++) {
            if (board[i].getStones() == 0) {
                emptyPit2++;
            }
        }
        if (emptyPit1 == 6 || emptyPit2 == 6) {
            return true;
        }
        return false;
    }

    public int countDifference(int whichPlayer, Pit[] board) {
        if (whichPlayer == 1) {
            return board[6].getStones() - board[13].getStones();
        } else return board[13].getStones() - board[6].getStones();

    }

    public int[] countResults() {
        int[] result = new int[2];
        if (getWinner() == 1) {
            int extraStonesForWinner = 0;
            for (int i = 0; i < 6; i++) {
                extraStonesForWinner = extraStonesForWinner + board[i].getStones();
                board[i].setStones(0);
            }
            board[6].setStones(board[6].getStones() + extraStonesForWinner);
            result[0] = board[6].getStones();
            result[1] = board[13].getStones();

        } else if (getWinner() == 2) {
            int extraStonesForWinner = 0;
            for (int i = 7; i < 13; i++) {
                extraStonesForWinner = extraStonesForWinner + board[i].getStones();
                board[i].setStones(0);
            }
            board[13].setStones(board[13].getStones() + extraStonesForWinner);
            result[1] = board[13].getStones();
            result[0] = board[6].getStones();
        }
        printBoard();
        return result;
    }

    public int getWinner() {
        int winner = 0;
        if (board[6].getStones() == board[13].getStones()) {
            winner = 0;
        }
        if (board[6].getStones() > board[13].getStones()) {
            winner = 1;
        } else winner = 2;
        return winner;
    }

    public void printResults() {
        int[] result = countResults();
        if (result[0] > result[1]) {
            System.out.println("Player 1 won with result: |" + result[0] + ":" + result[1] + "|");
        } else if (result[1] > result[0]) {
            System.out.println("Player 2 won with result: |" + result[1] + ":" + result[0] + "|");
        } else System.out.println("No winner, same result: |" + result[0] + ":" + result[1] + "|");
    }

    public void printBoard() {
        System.out.println("\n                          PLAYER 1                ");
        for (int i = 5; i >= 0; i--) {
            System.out.print("      " + board[i].toString());

        }
        System.out.println("\n" + board[6].toString() + "                                                           " + board[13].toString());

        for (int i = 7; i < 13; i++) {
            System.out.print("      " + board[i].toString());

        }
        System.out.println("\n                          PLAYER 2                \n");

    }


    public Pit[] getBoard() {
        return board;
    }

    public void setBoard(Pit[] board) {
        this.board = board;
    }

    public static Pit[] copyBoard(Pit[] board) {
        Pit[] copiedBoard = new Pit[board.length];
        for (int i = 0; i < board.length; i++) {
            copiedBoard[i] = new Pit(board[i]);
        }
        return copiedBoard;
    }

}
