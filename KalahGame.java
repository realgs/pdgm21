import java.util.Random;
import java.util.Scanner;

public class KalahGame 
{
	private Board gameBoard;
	private Board simulatedBoard;
	private Random random;
	private Scanner scanner;
	private boolean hasEnded;
	
	public KalahGame()
	{
		gameBoard = new Board();
		simulatedBoard = new Board();
		random = new Random();
		scanner = new Scanner(System.in);
		hasEnded = false;
	}
	
	public int getAIMoveFour(int playerId)
	{
		simulatedBoard.replaceWith(gameBoard);
		
		int[][][][] diffInPointsInFourMoves = new int [13][13][13][13];
		
		int maxDifference = Integer.MIN_VALUE;
		int maxDifferenceIndex = Integer.MIN_VALUE;
		
		if(playerId == 0)
		{
			for(int i = 0; i <= 5; i++)
			{
				for(int j = 7; j <= 12; j++)
				{
					for(int k = 0; k <= 5; k++)
					{
						for(int l = 7; l <= 12; l++)
						{
							if(!simulatedBoard.makeMove(i, 0) || 
									!simulatedBoard.makeMove(j, 1) ||
									!simulatedBoard.makeMove(k, 0) ||
									!simulatedBoard.makeMove(l, 1))
							{
								simulatedBoard.replaceWith(gameBoard);
								diffInPointsInFourMoves[i][j][k][l] = -1000;
								continue;
							}
							
							diffInPointsInFourMoves[i][j][k][l] = simulatedBoard.getFields()[simulatedBoard.getBaseIndexOfId(0)].getStones() -
									simulatedBoard.getFields()[simulatedBoard.getBaseIndexOfId(1)].getStones();
							simulatedBoard.replaceWith(gameBoard);
						}
					}
				}
			}
			
			for(int i = 0; i <= 5; i++)
			{
				for(int j = 7; j <= 12; j++)
				{
					for(int k = 0; k <= 5; k++)
					{
						for(int l = 7; l <= 12; l++)
						{
							if(diffInPointsInFourMoves[i][j][k][l] > maxDifference)
							{
								maxDifference = diffInPointsInFourMoves[i][j][k][l];
								maxDifferenceIndex = i;
							}
						}
					}
				}
			}
		}
		else
		{
			for(int i = 7; i <= 12; i++)
			{
				for(int j = 0; j <= 5; j++)
				{
					for(int k = 7; k <= 12; k++)
					{
						for(int l = 0; l <= 5; l++)
						{
							if(!simulatedBoard.makeMove(i, 1) || 
									!simulatedBoard.makeMove(j, 0) ||
									!simulatedBoard.makeMove(k, 1) ||
									!simulatedBoard.makeMove(l, 0))
							{
								simulatedBoard.replaceWith(gameBoard);
								diffInPointsInFourMoves[i][j][k][l] = -1000;
								
								continue;
							}
							
							diffInPointsInFourMoves[i][j][k][l] = simulatedBoard.getFields()[simulatedBoard.getBaseIndexOfId(1)].getStones() -
									simulatedBoard.getFields()[simulatedBoard.getBaseIndexOfId(0)].getStones();
							simulatedBoard.replaceWith(gameBoard);
						}
					}
				}
			}
			
			for(int i = 7; i <= 12; i++)
			{
				for(int j = 0; j <= 5; j++)
				{
					for(int k = 7; k <= 12; k++)
					{
						for(int l = 0; l <= 5; l++)
						{
							if(diffInPointsInFourMoves[i][j][k][l] > maxDifference)
							{
								maxDifference = diffInPointsInFourMoves[i][j][k][l];
								maxDifferenceIndex = i;
							}
						}
					}
				}
			}
		}
		
		if(maxDifference == -1000)
		{
			int randomIndex = random.nextInt(13);
			
			while(simulatedBoard.getFields()[randomIndex].getStones() == 0 &&
					simulatedBoard.getFields()[randomIndex].getPlayerId() == playerId)
				randomIndex = random.nextInt(13);
			
			return randomIndex;
		}
		
		return maxDifferenceIndex;
	}
	
	public void playAIvsAI()
	{
		int firstMoveId = random.nextInt(2);
		
		while(!hasEnded)
		{
			gameBoard.makeMove(getAIMoveFour(firstMoveId), firstMoveId);
			
			gameBoard.print();
			
			while(gameBoard.isAnotherMove())
			{
				gameBoard.makeMove(getAIMoveFour(firstMoveId), firstMoveId);
				
				gameBoard.print();
			}
			
			if(gameBoard.ifGetsOpponentStones())
			{
				Field tempField = gameBoard.getFields()[gameBoard.getLastField()];
				
				gameBoard.getFields()[gameBoard.getBaseIndexOfId(gameBoard.getLastMoveId())].
				addStones(gameBoard.getFields()[tempField.getOppositeField()].getStones());
						
				gameBoard.getFields()[tempField.getOppositeField()].removeStones();
			}
			
			hasEnded(Math.abs(firstMoveId - 1));
			
			if(hasEnded)
			{
				printScore();
				return;
			}
			
			gameBoard.makeMove(getAIMoveFour(Math.abs(firstMoveId - 1)), Math.abs(firstMoveId - 1));
			
			gameBoard.print();
			
			while(gameBoard.isAnotherMove())
			{
				gameBoard.makeMove(getAIMoveFour(Math.abs(firstMoveId - 1)), Math.abs(firstMoveId - 1));
				
				gameBoard.print();
			}
			
			if(gameBoard.ifGetsOpponentStones())
			{
				Field tempField = gameBoard.getFields()[gameBoard.getLastField()];
				
				gameBoard.getFields()[gameBoard.getBaseIndexOfId(gameBoard.getLastMoveId())].
				addStones(gameBoard.getFields()[tempField.getOppositeField()].getStones());
						
				gameBoard.getFields()[tempField.getOppositeField()].removeStones();
			}
			
			hasEnded(firstMoveId);
			
			if(hasEnded)
			{
				printScore();
				return;
			}
		}
	}
	
	public void playPlayerVsAI()
	{
		int playersChoice = 0;
		
		int firstMoveId = random.nextInt(2);
		
		if(firstMoveId == 1)
		{
			gameBoard.makeMove(getAIMoveFour(firstMoveId), firstMoveId);
			
			gameBoard.print();
			
			while(gameBoard.isAnotherMove())
			{
				gameBoard.makeMove(getAIMoveFour(firstMoveId), firstMoveId);
				
				gameBoard.print();
			}
			
			if(gameBoard.ifGetsOpponentStones())
			{
				Field tempField = gameBoard.getFields()[gameBoard.getLastField()];
				
				gameBoard.getFields()[gameBoard.getBaseIndexOfId(gameBoard.getLastMoveId())].
				addStones(gameBoard.getFields()[tempField.getOppositeField()].getStones());
						
				gameBoard.getFields()[tempField.getOppositeField()].removeStones();
			}
		}
		
		gameBoard.print();
		
		while(!hasEnded)
		{
			System.out.print("Choose from which field you wanna move: ");
			
			playersChoice = scanner.nextInt();
			
			while(!gameBoard.makeMove(playersChoice, 0))
			{
				System.out.print("Choose from which field you wanna move: ");
				
				playersChoice = scanner.nextInt();
			}
			
			gameBoard.print();
			
			while(gameBoard.isAnotherMove())
			{
				System.out.print("Choose from which field you wanna move: ");
				
				playersChoice = scanner.nextInt();
				
				while(!gameBoard.makeMove(playersChoice, 0))
				{
					System.out.print("Choose from which field you wanna move: ");
					
					playersChoice = scanner.nextInt();
				}
				
				gameBoard.print();
			}
			
			if(gameBoard.ifGetsOpponentStones())
			{
				Field tempField = gameBoard.getFields()[gameBoard.getLastField()];
				
				gameBoard.getFields()[gameBoard.getBaseIndexOfId(gameBoard.getLastMoveId())].
				addStones(gameBoard.getFields()[tempField.getOppositeField()].getStones());
						
				gameBoard.getFields()[tempField.getOppositeField()].removeStones();
			}
			
			hasEnded(1);
			
			if(hasEnded)
			{
				printScore();
				return;
			}
			
			gameBoard.makeMove(getAIMoveFour(1), 1);
			
			gameBoard.print();
			
			while(gameBoard.isAnotherMove())
			{
				gameBoard.makeMove(getAIMoveFour(1), 1);
				
				gameBoard.print();
			}
			
			if(gameBoard.ifGetsOpponentStones())
			{
				Field tempField = gameBoard.getFields()[gameBoard.getLastField()];
				
				gameBoard.getFields()[gameBoard.getBaseIndexOfId(gameBoard.getLastMoveId())].
				addStones(gameBoard.getFields()[tempField.getOppositeField()].getStones());
						
				gameBoard.getFields()[tempField.getOppositeField()].removeStones();
			}
			
			hasEnded(0);
			
			if(hasEnded)
			{
				printScore();
				return;
			}
		}
	}
	
	private void printScore()
	{
		int ai1pts = gameBoard.getFields()[0].getStones() + gameBoard.getFields()[1].getStones() +
				gameBoard.getFields()[2].getStones() + gameBoard.getFields()[3].getStones() + 
				gameBoard.getFields()[4].getStones() + gameBoard.getFields()[5].getStones() + 
				gameBoard.getFields()[6].getStones();
		int ai2pts = gameBoard.getFields()[7].getStones() + gameBoard.getFields()[8].getStones() +
				gameBoard.getFields()[9].getStones() + gameBoard.getFields()[10].getStones() + 
				gameBoard.getFields()[11].getStones() + gameBoard.getFields()[12].getStones() + 
				gameBoard.getFields()[13].getStones();
		
		System.out.println("Points of PlayerId0: " + ai1pts);
		System.out.println("Points of PlayerId1: " + ai2pts);
		
		if (ai2pts > ai1pts)
			System.out.println("PlayerId1 has won!");
		else if (ai2pts < ai1pts)
			System.out.println("PlayerId0 has won!");
		else
			System.out.println("Draw!");
	}
	
	public void hasEnded(int playerToCheck)
	{
		if(playerToCheck == 1)
		{
			if (gameBoard.getFields()[7].getStones() == 0 && 
					gameBoard.getFields()[8].getStones() == 0 && 
					gameBoard.getFields()[9].getStones() == 0 && 
					gameBoard.getFields()[10].getStones() == 0 && 
					gameBoard.getFields()[11].getStones() == 0 && 
					gameBoard.getFields()[12].getStones() == 0)
			{
				System.out.println("PlayerId1 cannot make a move! Let's count the points!\n");
				gameBoard.print();
				System.out.println();
			    hasEnded = true;
			}
		}
		else
		{
			if (gameBoard.getFields()[0].getStones() == 0 && 
					gameBoard.getFields()[1].getStones() == 0 && 
					gameBoard.getFields()[2].getStones() == 0 && 
					gameBoard.getFields()[3].getStones() == 0 && 
					gameBoard.getFields()[4].getStones() == 0 && 
					gameBoard.getFields()[5].getStones() == 0)
			{
				System.out.println("PlayerId0 cannot make a move! Let's count the points!\n");
				gameBoard.print();
				System.out.println();
				hasEnded = true;
			}
		}
	}
}
