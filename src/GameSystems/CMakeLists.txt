set(GAMESYSTEMS_SOURCES System.cpp Server.cpp Client.cpp IPublisher.cpp ISubscriber.cpp HumanClient.cpp BotClient.cpp)

add_library(LIB_GAMESYSTEMS ${GAMESYSTEMS_SOURCES})

target_include_directories(LIB_GAMESYSTEMS PUBLIC ${INCLUDES_DIR}/GameSystems ${INCLUDES_DIR}/GameUtils)

target_link_libraries(LIB_GAMESYSTEMS PRIVATE LIB_GAMEUTILS LIB_GAMEUSERS LIB_GAMEOBJECTS)