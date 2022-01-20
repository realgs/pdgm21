public class Board 
{
	private Field[] fields = new Field[14];	
	private int lastField;
	private int lastMoveId;
	
	public Board()
	{
		fields[6] = new Field(true, 0);
		fields[13] = new Field(true, 1);
		
		for(int i = 0; i <= 5; i++)
		{
			fields[i] = new Field(false, 0);
			fields[i].setOppositeField(12 - i);
		}
		
		for(int i = 7; i <= 12; i++)
		{
			fields[i] = new Field(false, 1);
			fields[i].setOppositeField(12 - i);
		}
		
		lastField = 0;
	}
	
	public boolean makeMove(int fromField, int playerId)
	{
		if(playerId != fields[fromField].getPlayerId() || fields[fromField].getStones() == 0)
		{
			return false;
		}
		
		for(int i = 1; i <= fields[fromField].getStones(); i++)
		{
			fields[(fromField + i) % 14].addStone();
		}
		
		lastField = (fromField + fields[fromField].getStones()) % 14;
		
		fields[fromField].removeStones();
		
		lastMoveId = playerId;
		
		return true;
	}
	
	public boolean isAnotherMove()
	{
		if(fields[lastField].isBase() && fields[lastField].getPlayerId() == lastMoveId)
		{
			if(lastMoveId == 0)
			{
				if(fields[0].getStones() + fields[1].getStones() + fields[2].getStones() + 
						fields[3].getStones() + fields[4].getStones() + fields[5].getStones() == 0)
				{
					return false;
				}
			}
			else
			{
				if(fields[7].getStones() + fields[8].getStones() + fields[9].getStones() + 
						fields[10].getStones() + fields[11].getStones() + fields[12].getStones() == 0)
				{
					return false;
				}
			}
			
			System.out.println("\nYou have extra move!");
			return true;
		}
		else
			return false;
	}
	
	public boolean ifGetsOpponentStones()
	{
		if(fields[lastField].getStones() - 1 == 0 && fields[lastField].getPlayerId() == lastMoveId)
		{
			System.out.println("\nYou get opponent stones!");
			return true;
		}
		else
			return false;
	}
	
	public void print()
	{
		System.out.println("\nBOARD STATUS: ");
	    System.out.printf("    %5s%5s%5s%5s%5s%5s%n", "(12)", "(11)", "(10)", "(9)", "(8)", "(7)");
	    System.out.printf("(13)%4d%5d%5d%5d%5d%5d   (6)%n", fields[12].getStones(), fields[11].getStones(),
	    		fields[10].getStones(), fields[9].getStones(), fields[8].getStones(), fields[7].getStones());
	    System.out.printf("%3d    --------------------------%5d%n", fields[13].getStones(), fields[6].getStones());
	    System.out.printf("   %5d%5d%5d%5d%5d%5d%n", fields[0].getStones(), fields[1].getStones(), 
	    		fields[2].getStones(), fields[3].getStones(), fields[4].getStones(), 
	    		fields[5].getStones());
	    System.out.printf("    %5s%5s%5s%5s%5s%5s%n", "(0)", "(1)", "(2)", "(3)", "(4)", "(5)");
	}
	
	public void replaceWith(Board board2)
	{
		for(int i = 0; i < 14; i++)
		{
			fields[i].setStones(board2.getFields()[i].getStones());
		}
	}
	
	public int getBaseIndexOfId(int playerId)
	{
		if(playerId == 0)
			return 6;
		else if(playerId == 1)
			return 13;
		else
		{
			System.out.println("Wrong player ID!");
			return -1;
		}
	}
	
	public int getLastField()
	{
		return lastField;
	}
	
	public int getLastMoveId()
	{
		return lastMoveId;
	}
	
	public Field[] getFields()
	{
		return fields;
	}
}
