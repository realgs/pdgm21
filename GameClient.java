package io.github.reconsolidated;

import lombok.Getter;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

public class GameClient {

    @Getter
    protected int ID = 0;

    protected GameServer server = null;
    protected List<ServerPacket> waitingPackets;

    protected Map<Integer, Integer> holes;

    protected int turn = 0;

    public GameClient() {
        waitingPackets = new ArrayList<>();
        holes = new HashMap<>();
    }

    public void connect(GameServer server) {
        ID = server.connect(this);
        this.server = server;
    }

    public void receivePacket(ServerPacket packet) {
        switch (packet.getType()) {
            case STATE_UPDATE -> {
                holes = (Map<Integer, Integer>) packet.getArguments().get("holes");
                turn = (Integer) packet.getArguments().get("turn");
            }
        }
    }



    public void move(int hole) {
        hole -= 1;
        if (ID == 1) {
            hole += 7;
        }

        Map<String, Object> arguments = new HashMap<>();
        arguments.put("ID", ID);
        arguments.put("hole", hole);
        server.receivePacket(this, new ServerPacket(ServerPacketType.MOVE, arguments));
    }

    public String getState() {
        StringBuilder s = new StringBuilder("ID: " + ID + ", Turn: " + turn + ", holes: \n    ");
        for (int i = 12; i>6; i--) {
            s.append(holes.get(i)).append(" ");
        }
        s.append("\n");
        s.append(holes.get(13)).append("                ").append(holes.get(6));
        s.append("\n    ");
        for (int i = 0; i<6; i++) {
            s.append(holes.get(i)).append(" ");
        }
        return s.toString();
    }


}
