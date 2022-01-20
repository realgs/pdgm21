package Kalah;

public class Board {
    private Hole[] board;


    public Board() {
        board = new Hole[14];
        int counter = 0;
        int opposite = 12;
        for (int i = 0; i < board.length; i++) {
            if (i < 6) {
                board[i] = new Hole("P1", 6, counter++, 1, opposite--);
            } else if (i == 6) {
                board[i] = new Hole("B1", 0, counter++, 1, -1);
                counter = 0;
                opposite = 5;
            } else if (i < 13) {
                board[i] = new Hole("P2", 6, counter++, 2, opposite--);
            } else {
                board[i] = new Hole("B2", 0, counter++, 2, -2);
            }
        }
    }

    public Board(Hole[] board) {
        this.board = board;
    }

    public boolean moveStones(int position, int team) {
        if (team == 2) {
            position = position + 7;
        }
        int stones = board[position].getStones();
        board[position].setStones(0);
        position++;
        while (stones > 0) {
            if (team == 1 && position == 13) {
                position = 0;
            }
            if (team == 2 && position == 14) {
                position = 0;
            }
            if (team == 2 && position == 6) {
                position = 7;
            }
            board[position].setStones(board[position].getStones() + 1);
            position++;
            stones--;
        }
        bonus(position - 1, team);
        if (team == 1 && position - 1 == 6 || team == 2 && position - 1 == 13) { //bonus move
            return true;
        }
        return false; //no bonus move
    }


    public void showBoard() {
        System.out.println("\n\n\n\t\t\t\t\t\tPLAYER 1");
        System.out.print("\t\t");
        for (int i = 5; i >= 0; i--) {
            System.out.print(board[i].toString() + "\t");
        }
        System.out.print("\n" + board[6].toString() + "\t\t\t\t\t\t\t\t\t\t\t\t\t");
        System.out.println(board[13].toString());
        System.out.print("\t\t");
        for (int i = 7; i < 13; i++) {
            System.out.print(board[i].toString() + "\t");
        }
        System.out.println("\n\t\t\t\t\t\tPLAYER 2");
    }

    

    /*public void showBoard() {
        System.out.println("\n\n\n\t\t\tPLAYER 1");
        System.out.print("\t");
        for (int i = 5; i >= 0; i--) {
            System.out.print(board[i].toString() + "\t");
        }
        System.out.print("\n" + board[6].toString() + "\t\t\t\t\t\t\t");
        System.out.println(board[13].toString());
        System.out.print("\t");
        for (int i = 7; i < 13; i++) {
            System.out.print(board[i].toString() + "\t");
        }
        System.out.println("\n\t\t\tPLAYER 2");
    }
    */

    public void bonus(int position, int team) {
        if (team == 1 && position < 6) {
            if (board[position].getStones() == 1) {
                board[6].setStones(board[board[position].getOppositeId()].getStones() + board[6].getStones());
                board[board[position].getOppositeId()].setStones(0);
            }
        }
        if (team == 2 && position > 6 && position < 13) {
            if (board[position].getStones() == 1) {
                board[13].setStones(board[board[position].getOppositeId()].getStones() + board[13].getStones());
                board[board[position].getOppositeId()].setStones(0);
            }
        }
    }

    public boolean isOver() {
        int howManyEmptyTeam1 = 0;
        int howManyEmptyTeam2 = 0;
        for (int i = 0; i < 6; i++) {
            if (board[i].getStones() == 0) {
                howManyEmptyTeam1++;
            }
        }
        for (int i = 7; i < 13; i++) {
            if (board[i].getStones() == 0) {
                howManyEmptyTeam2++;
            }
        }
        if (howManyEmptyTeam1 == 6) {
            int bonus = 0;
            for (int i = 7; i < 13; i++) {
                bonus = bonus + board[i].getStones();
                board[i].setStones(0);
            }
            board[13].setStones(board[13].getStones() + bonus);
            return true;
        }
        if (howManyEmptyTeam2 == 6) {
            int bonus = 0;
            for (int i = 0; i < 6; i++) {
                bonus = bonus + board[i].getStones();
                board[i].setStones(0);
            }
            board[6].setStones(board[6].getStones() + bonus);
            return true;
        }
        return false;
    }

    public int whoWon() {
        int winner;
        if (board[6].getStones() == board[13].getStones()) {
            winner = 0;
        } else if (board[6].getStones() > board[13].getStones()) {
            winner = 1;
        } else winner = 2;

        return winner;
    }

    public static Hole[] copyBoard(Hole[] board) {
        Hole[] boardCopy = new Hole[board.length];
        for (int i = 0; i < board.length; i++) {
            boardCopy[i] = new Hole(board[i]);
        }
        return boardCopy;
    }

    public int nextTeam(int team) {
        int result = 0;
        if (team == 1) {
            result = 2;
        } else result = 1;
        return result;
    }


    public int calculateTheDifference(int team, Hole[] board) {
        if (team == 1) {
            return board[6].getStones() - board[13].getStones();
        } else return board[13].getStones() - board[6].getStones();
    }


    public Hole[] getBoard() {
        return board;
    }

    public void setBoard(Hole[] board) {
        this.board = board;
    }
}

