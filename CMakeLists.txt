cmake_minimum_required(VERSION 3.16)

set(PROJECT_NAME "List 8")
set(CMAKE_CXX_FLAGS "-g -Wall -Werror -pthread")
set(CMAKE_CXX_STANDARD 20)

project(${PROJECT_NAME}
        DESCRIPTION "This project implements Kalaha game"
        VERSION 1.0
        LANGUAGES CXX)

set(INCLUDES_DIR "${PROJECT_SOURCE_DIR}/inc/")
set(INCLUDES_GAMELOGIC_DIR "${INCLUDES_DIR}/GameLogic")

add_subdirectory(src)

set(MAIN_APP_SOURCES main.cpp inc/GameLogic/GameSystems/IRemoteSession.hpp inc/GameLogic/GameSystems/ILocalHandler.hpp inc/GameLogic/GameSystems/IRemoteHandler.hpp inc/GameLogic/GameSystems/ILocalSession.hpp inc/GameLogic/GameSystems/ISession.hpp inc/GameLogic/GameSystems/IHandler.hpp inc/GameLogic/GameSystems/GameSessionSate.hpp inc/GameLogic/GameSystems/PlayerMoveState.hpp inc/GameLogic/GameSystems/PlayerMove.hpp)
set(MAIN_APP APP)

add_executable(${MAIN_APP} ${MAIN_APP_SOURCES})

target_include_directories(${MAIN_APP} PRIVATE ${INCLUDES_GAMELOGIC_DIR}/GameSystems)

target_link_libraries(${MAIN_APP} PRIVATE LIB_GAMESYSTEMS)

#find cppzmq wrapper, installed by make of cppzmq
find_package(cppzmq)
target_link_libraries(${MAIN_APP} PRIVATE cppzmq)
