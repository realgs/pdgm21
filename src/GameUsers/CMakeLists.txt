set(GAMEUSERS_SOURCES HumanPlayer.cpp BotPlayer.cpp Player.cpp PlayerScore.cpp)

add_library(LIB_GAMEUSERS ${GAMEUSERS_SOURCES})

target_include_directories(LIB_GAMEUSERS PUBLIC "${INCLUDES_DIR}/GameUsers")

target_link_libraries(LIB_GAMEUSERS PRIVATE LIB_GAMESYSTEMS LIB_GAMEOBJECTS)