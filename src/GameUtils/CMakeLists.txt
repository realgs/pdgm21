set(GAMEUTILS_SOURCES PlayerSettings.cpp Settings.cpp)

add_library(LIB_GAMEUTILS ${GAMEUTILS_SOURCES})

target_include_directories(LIB_GAMEUTILS PUBLIC "${INCLUDES_DIR}/GameUtils")

target_link_libraries(LIB_GAMEUTILS PRIVATE LIB_GAMEUSERS)