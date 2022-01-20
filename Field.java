public class Field 
{
	private int stones;
	private int playerId;
	private boolean isBase;
	private int oppositeField;
	
	public Field()
	{
		isBase = false;
		stones = 6;
		playerId = 0;
		oppositeField = 0;
	}
	
	public Field(boolean isBase, int playerId)
	{
		this.isBase = isBase;
		
		if(isBase)
			stones = 0;
		else
			stones = 6;

		this.playerId = playerId;
		
		oppositeField = 0;
	}
	
	public void addStone()
	{
		stones += 1;
	}
	
	public void addStones(int stonesToAdd)
	{
		stones += stonesToAdd;
	}
	
	public void removeStones()
	{
		stones = 0;
	}
	
	public int getPlayerId()
	{
		return playerId;
	}
	
	public int getStones()
	{
		return stones;
	}
	
	public boolean isBase()
	{
		return isBase;
	}
	
	public void setOppositeField(int oppositeField)
	{
		this.oppositeField = oppositeField;
	}
	
	public int getOppositeField()
	{
		return oppositeField;
	}
	
	public void setStones(int stones)
	{
		this.stones = stones;
	}
}
