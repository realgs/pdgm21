package kalahaGame;

public class KalahaGame {
	
	private int[] board;
	int player;
	
	public KalahaGame(int stones) {
		board = new int[14];
		player = 0;
		
		for (int i = 0; i < board.length; i++) {
			if (i != 6 && i != 13)
				board[i] = stones;
			else
				board[i] = 0;
		}
	}
	
	public String getBoardToPrint() {
		String boardToPrint = "\n  |  ";
		
		for (int i = 12; i >= 7; i--)
			boardToPrint = boardToPrint.concat(Integer.toString(board[i]) + "  ");
		
		boardToPrint = boardToPrint.concat("|\n--|--------------------|--\n  |  ");
		
		for (int i = 6; i > 0; i--)
			boardToPrint = boardToPrint.concat(Integer.toString(i) + "  ");
		
		boardToPrint = boardToPrint.concat("|\n" + Integer.toString(board[13]));
		boardToPrint = boardToPrint.concat(" |                    | " + Integer.toString(board[6]) + "\n  |  ");
		
		for (int i = 1; i < 7; i++)
			boardToPrint = boardToPrint.concat(Integer.toString(i) + "  ");
		
		boardToPrint = boardToPrint.concat("|\n--|--------------------|--\n  |  ");
		
		for (int i = 0; i < 6; i++)
			boardToPrint = boardToPrint.concat(Integer.toString(board[i]) + "  ");
		
		boardToPrint = boardToPrint.concat("|\n\n");
		
		return boardToPrint;
	}
	
	public int makeMove(int move) {	
		if ((move >= 1 && move < 7) && board[player % 2 == 0 ? move - 1 : move + 6] != 0) {
			if (executeMove(player % 2 == 0 ? move - 1 : move + 6))
				return 1;
			else {
				player++;
				return 0;
			}			
		}
		else 
			return -1;	
	}
	
	public boolean ifEndOfGame() {
		boolean ifEnd = true;
		
		for (int i = 0; i < 6; i++)
			if (board[i] != 0)
				ifEnd = false;
		
		if (ifEnd)
			return true;
		
		ifEnd = true;
		
		for (int i = 7; i < 13; i++)
			if (board[i] != 0)
				ifEnd = false;
		
		return ifEnd;
	}
	
	public String getResults() {
		for (int i = 0; i < 6; i++)
			board[6] += board[i];
		
		for (int i = 7; i < 13; i++)
			board[13] += board[i];
		
		String winner;
		
		if (board[6] > board[13])
			winner = "Player 1 won!";
		else if (board[6] == board[13])
			winner = "Draw!";
		else
			winner = "Player 2 won!";
		
		return "Results:\nPlayer 1: " + board[6] + " Player 2: " + board[13] + "\n" + winner;
	}
	
	public int[] getBoardCopy() {
		return board.clone();
	}
	
	private boolean executeMove(int move) {
		int steps = board[move];
		board[move] = 0;
		int index = move;
		
		for (int i = 0; i < steps; i++) {
			if (++index > 13)
				index = 0;
			
			board[index]++;
		}
		
		if (index != 6 && index != 13 && board[index] == 1 && board[12 - index] != 0) {
			if (index < 6 && player % 2 == 0) {
				board[6] += board[12 - index];
				board[6]++;
				board[12 - index] = 0;
				board[index] = 0;
			}
			else if (index > 6 && player % 2 == 1){
				board[13] += board[12 - index];
				board[13]++;
				board[12 - index] = 0;
				board[index] = 0;
			}
		} else if ((index == 6 && player % 2 == 0) || (index == 13 && player % 2 == 1)) {
			if (!ifEndOfGame()) 
				return true;
		}
		
		return false;
	}
}
