package io.github.reconsolidated;

import lombok.Getter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GameServer implements Runnable {
    private Map<Integer, GameClient> connectedClients;
    private List<ServerPacket> waitingPackets;

    private boolean stop = false;

    @Getter
    private Map<Integer, Integer> holes;

    @Getter
    private int turn = -1;

    public GameServer() {
        connectedClients = new HashMap<>();
        waitingPackets = new ArrayList<>();
        holes = new HashMap<>();
        for (int i = 0; i<14; i++) {
            holes.put(i, 0);
        }
    }

    /**
     * @param client - game client that wants to connect
     * @return ID, null if connection not successful
     */
    public Integer connect(GameClient client) {
        if (connectedClients.containsKey(0) && connectedClients.containsKey(1)) {
            return null;
        }
        if (!connectedClients.containsKey(0)) {
            connectedClients.put(0, client);
            return 0;
        } else {
            connectedClients.put(1, client);
            return 1;
        }
    }

    public void receivePacket(GameClient sender, ServerPacket packet) {
        packet.setSender(sender);
        waitingPackets.add(packet);
    }

    public void run() {
        boolean shouldChangeTurn = false;
        boolean anythingChanged = false;

        for (int p = 0; p<waitingPackets.size(); p++) {
            ServerPacket packet = waitingPackets.get(p);
            waitingPackets.remove(p--);
            switch (packet.getType()) {
                case MOVE -> {
                    Integer id = (Integer) packet.getArguments().get("ID");
                    if (packet.getSender().getID() == id) {
                        if (turn == id) {
                            Integer hole = (Integer) packet.getArguments().get("hole");
                            if (getHoleOwner(hole) == id) {
                                Integer ballsInHole = holes.get(hole);
                                if (ballsInHole > 0) {
                                    shouldChangeTurn = true;
                                    anythingChanged = true;
                                    holes.put(hole, 0);
                                    for (int i = hole+1; i<ballsInHole+hole+1; i++) {
                                        Integer current = holes.get(i%14);
                                        if (i < ballsInHole+hole) {
                                            holes.put(i%14, current+1);
                                        } else {
                                            if (i%14 == getBase(id)) {
                                                holes.put(i%14, 1);
                                                shouldChangeTurn = false;
                                            } else if (current == 0 && getHoleOwner(i%14) == id) {
                                                int opposite = getOppositeHole(i%14);
                                                Integer taken = holes.get(opposite);
                                                holes.put(opposite, 0);
                                                int currentInBase = holes.get(getBase(id));
                                                holes.put(getBase(id), currentInBase + taken);
                                                holes.put(i%14, 1);
                                            } else {
                                                holes.put(i%14, current+1);
                                            }

                                        }
                                    }
                                }
                            }

                        }
                    }
                }
            }
        }

        checkForGameEnd();

        if (shouldChangeTurn) {
            turn = (turn+1)%2;
        }

        if (anythingChanged) {
            for (GameClient client : connectedClients.values()) {
                client.receivePacket(getStatePacket());
            }
        }
    }

    private void checkForGameEnd() {
        int sum = 0;
        for (int i = 0; i<6; i++) {
            int value = holes.get(i);
            sum += value;
        }
        if (sum == 0) {
            onGameEnd();
        }
        sum = 0;
        for (int i = 7; i<13; i++) {
            int value = holes.get(i);
            sum += value;
        }
        if (sum == 0) {
            onGameEnd();
        }
    }

    private void onGameEnd(int winnerID) {
        System.out.println("Winner is: " + winnerID);
    }

    private void onGameEnd() {
        int score1 = holes.get(6);
        int score2 = holes.get(13);
        if (score1 == score2) {
            onGameEnd(-1);
        }
        else if (score1 > score2) {
            onGameEnd(0);
        }
        else {
            onGameEnd(1);
        }
    }

    public void startGame() {
        for (int i = 0; i<14; i++) {
            holes.put(i, 6);
        }

        holes.put(6, 0);
        holes.put(13, 0);

        turn = 0;

        for (GameClient client : connectedClients.values()) {
            client.receivePacket(getStatePacket());
        }
    }

    private ServerPacket getStatePacket() {
        Map<String, Object> arguments = new HashMap<>();
        arguments.put("holes", new HashMap<>(holes));
        arguments.put("turn", turn);
        return new ServerPacket(ServerPacketType.STATE_UPDATE, arguments);
    }

    private int getBase(int id) {
        if (id == 0) return 6;
        return 13;
    }

    public boolean hasStarted() {
        return turn >= 0;
    }

    private int getHoleOwner(int hole) {
        if (hole >= 0 && hole < 6) return 0;
        if (hole >= 7 && hole < 13) return 1;
        return -1;
    }

    private int getOppositeHole(int hole) {
        return (hole + 7)%14;
    }


}
