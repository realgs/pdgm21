package io.github.reconsolidated;

import lombok.Getter;
import lombok.Setter;

import java.util.Map;

public class ServerPacket {

    @Getter
    private final ServerPacketType type;
    @Getter
    private final Map<String, Object> arguments;

    @Getter
    @Setter
    private GameClient sender;

    public ServerPacket(ServerPacketType type, Map<String, Object> arguments) {
        this.type = type;
        this.arguments = arguments;
    }
}
