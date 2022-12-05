set(GAMESYSTEMS_SOURCES GamesHandler.cpp GameSession.cpp ISession.cpp IHandler.cpp ILocalHandler.cpp IRemoteHandler.cpp ILocalSession.cpp IRemoteSession.cpp)

add_library(LIB_GAMESYSTEMS ${GAMESYSTEMS_SOURCES})

target_include_directories(LIB_GAMESYSTEMS PUBLIC ${INCLUDES_GAMELOGIC_DIR}/GameSystems ${INCLUDES_GAMELOGIC_DIR}/GameUtils ${INCLUDES_GAMELOGIC_DIR}/GameUsers)

target_link_libraries(LIB_GAMESYSTEMS PRIVATE LIB_GAMEUTILS LIB_GAMEUSERS)