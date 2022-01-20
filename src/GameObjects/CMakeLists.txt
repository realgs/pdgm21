set(GAMEOBJECTS_SOURCES Hole.cpp Rock.cpp House.cpp Mankala.cpp Board.cpp)

add_library(LIB_GAMEOBJECTS ${GAMEOBJECTS_SOURCES})

target_include_directories(LIB_GAMEOBJECTS PUBLIC "${INCLUDES_DIR}/GameObjects")

target_link_libraries(LIB_GAMEOBJECTS PRIVATE LIB_GAMEUSERS LIB_GAMESYSTEMS)