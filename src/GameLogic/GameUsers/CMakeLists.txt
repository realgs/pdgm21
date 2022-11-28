set(GAMEUSERS_SOURCES HumanPlayer.cpp BotPlayer.cpp Player.cpp ../GameUtils/DecisionTree.cpp)

add_library(LIB_GAMEUSERS ${GAMEUSERS_SOURCES})

target_include_directories(LIB_GAMEUSERS PUBLIC "${INCLUDES_GAMELOGIC_DIR}/GameUsers")

target_link_libraries(LIB_GAMEUSERS PRIVATE LIB_GAMESYSTEMS LIB_GAMEUTILS)