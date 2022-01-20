package Kalah;

public class Hole {
    private int stones;
    private int id;
    private int team;
    private int oppositeId;

    public Hole(String name, int stones, int id, int team, int oppositeId) {
        this.stones = stones;
        this.id = id;
        this.team = team;
        this.oppositeId = oppositeId;
    }

    public Hole(Hole anotherNode) {
        this.stones = anotherNode.stones;
        this.id = anotherNode.id;
        this.team = anotherNode.team;
        this.oppositeId = anotherNode.oppositeId;
    }

    public int getId() {
        return id;
    }

    public int getOppositeId() {
        return oppositeId;
    }

    public int getStones() {
        return stones;
    }

    public int getTeam() {
        return team;
    }

    public void setId(int id) {
        this.id = id;
    }

    public void setOppositeId(int oppositeId) {
        this.oppositeId = oppositeId;
    }

    public void setStones(int stones) {
        this.stones = stones;
    }

    public void setTeam(int team) {
        this.team = team;
    }

    @Override
    public String toString() {
        return id + "[" + stones + "]";
    }
}

