set(GAMESYSTEMS_SOURCES GamesHandler.cpp GameSession.cpp IPublisher.cpp IClient.cpp RemoteClient.cpp LocalClient.cpp)

add_library(LIB_GAMESYSTEMS ${GAMESYSTEMS_SOURCES})

target_include_directories(LIB_GAMESYSTEMS PUBLIC ${INCLUDES_GAMELOGIC_DIR}/GameSystems ${INCLUDES_GAMELOGIC_DIR}/GameUtils)

target_link_libraries(LIB_GAMESYSTEMS PRIVATE LIB_GAMEUTILS LIB_GAMEUSERS)