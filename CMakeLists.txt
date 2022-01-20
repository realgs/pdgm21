cmake_minimum_required(VERSION 3.16)

set(PROJECT_NAME "List 8")
#set(CMAKE_CXX_FLAGS "-g -Wall -Werror")
set(CMAKE_CXX_STANDARD 20)

project(${PROJECT_NAME}
        DESCRIPTION "This project implements Kalaha game"
        VERSION 1.0
        LANGUAGES CXX)

set(INCLUDES_DIR "${PROJECT_SOURCE_DIR}/inc/")
add_subdirectory(src)

set(MAIN_APP_SOURCES main.cpp)
set(MAIN_APP APP)

add_executable(${MAIN_APP} ${MAIN_APP_SOURCES})

target_include_directories(${MAIN_APP} PRIVATE ${INCLUDES_DIR}/GameObjects ${INCLUDES_DIR}/GameUsers ${INCLUDES_DIR}/GameSystems)

target_link_libraries(${MAIN_APP} PRIVATE LIB_GAMEOBJECTS LIB_GAMEUSERS LIB_GAMESYSTEMS)
