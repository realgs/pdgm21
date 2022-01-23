//
// Author: jakubszwedowicz
// Date: 21.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include <iostream>
#include <vector>
#include <string>

#include "GameState.hpp"

GameState::GameState(const std::vector<std::string>& a_playerNames, int a_numberOfHumanPlayers,
                     int a_numberOfBotPlayers, int a_numberOfStartingRocks, int a_housesPerSide)
        :
        m_numberOfHumanPlayers(a_numberOfHumanPlayers)
        , m_numberOfBotPlayers(a_numberOfBotPlayers)
        , m_numberOfPlayers(m_numberOfHumanPlayers + m_numberOfBotPlayers)
        , m_housesPerSide(a_housesPerSide)
        , m_holesPerSide(m_housesPerSide + 1)
        , m_holesOnBoard(m_numberOfPlayers * m_holesPerSide)
        , m_numberOfStartingRocks(a_numberOfStartingRocks)
        , m_rocksLeft(m_numberOfPlayers, m_housesPerSide * m_numberOfStartingRocks)
        , m_points(m_numberOfPlayers, 0)
        , m_playerNames(a_playerNames)
{
    for (int i = 0; i < m_numberOfPlayers; i++)
    {
        for (int j = 0; j < m_housesPerSide; j++)
        {
            m_board.push_back(m_numberOfStartingRocks);
        }
        m_board.push_back(0);
    }
}

std::ostream& operator<<(std::ostream& a_out, const GameState& a_gameState) noexcept
{   //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |
    //|0 |-----------------------| 0|
    //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |


    //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |
    a_out << "|   |";
    for (int i = a_gameState.m_holesOnBoard - 2; i >= a_gameState.m_holesPerSide; i--)
        a_out << " " << a_gameState.m_board[i] << " |";
    a_out << "   |" << std::endl;


    //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |
    //|0 |-----------------------| 0|
    a_out << "| " << a_gameState.m_board[a_gameState.m_holesOnBoard - 1] << " |";
    for (int i = 0; i < a_gameState.m_housesPerSide - 1; i++)
        a_out << "----";
    a_out << "---| " << a_gameState.m_board[a_gameState.m_housesPerSide] << " |" << std::endl;

    //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |
    //|0 |-----------------------| 0|
    //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |
    a_out << "|   |";
    for (int i = 0; i < a_gameState.m_housesPerSide; i++)
        a_out << " " << a_gameState.m_board[i] << " |";
    a_out << "   |" << std::endl;

    return a_out;
}

int GameState::unloadHouse(int a_house, int a_player) noexcept
{
    int rocks = std::exchange(m_board[a_house], 0);
    m_rocksLeft[a_player] -= rocks;

    int opponentPlayer = (a_player + 1) % m_numberOfPlayers;
    for (int i = rocks; i != 0; i--)
    {
        a_house++;
        if (a_house == m_holesOnBoard) a_house = 0;

        if ((a_house + 1) % m_holesPerSide == 0) // mankala
        {
            if ((a_player + 1) * m_holesPerSide - 1 == a_house)  // a_player mankala
            {
                m_points[a_player]++;
            } else  // Opponent Mankala
            {
                i++;
                continue;
            }
        } else
        {
            if (a_house >= a_player * m_holesPerSide &&
                a_house < (a_player + 1) * m_holesPerSide - 1)   // a_players house
            {
                m_rocksLeft[a_player]++;    // Adding rocks to the a_player
                if(m_board[a_house] == 0 && i == 1)   // a_player's last rock put into his/her empty house
                {
                    int oppositeHouseIndex = a_house + 2 * ((m_holesPerSide - 1) - a_house);
                    int stolenRocks = std::exchange(m_board[oppositeHouseIndex], 0);
                    m_rocksLeft[opponentPlayer] -= stolenRocks;
                    m_board[(a_player + 1) * m_holesPerSide - 1] += stolenRocks;
                    m_points[a_player] += stolenRocks;
                }
            } else
            {
                int nextPlayer = (a_player + 1) % m_numberOfPlayers;
                m_rocksLeft[nextPlayer]++;
            }
        }
        m_board[a_house]++;
    }
//    if (m_rocksLeft[a_player] == 0) // Game ends when player that is about to move
//    {
//        finishGame(a_player);
//        return -1;
//    }

    return a_house;
}


void GameState::finishGame(int a_finishingPlayer) noexcept
{
    int opponentPlayer = (a_finishingPlayer + 1) % m_numberOfPlayers;
    int opponentPlayerHouse = opponentPlayer * m_holesPerSide;
    int finishingPlayerMankala = (a_finishingPlayer + 1) * m_holesPerSide - 1;
    for (int i = 0; i < m_holesPerSide - 1; i++)
    {
        int rocks = std::exchange(m_board[opponentPlayerHouse + i], 0);
        m_board[finishingPlayerMankala] += rocks;
    }
    m_points[a_finishingPlayer] = m_board[finishingPlayerMankala];
    m_rocksLeft[opponentPlayer] = 0;
    m_rocksLeft[a_finishingPlayer] = 0;
}

bool GameState::validateMove(int a_move, int a_playerIndex) const noexcept
{
    return a_move >= a_playerIndex * m_holesPerSide
           && a_move < (a_playerIndex + 1) * m_holesPerSide - 1
           && m_board[a_move] != 0;
}

void GameState::updateRocksPointsBoardWithOther(GameState&& a_gameState) noexcept
{
    m_rocksLeft = std::move(a_gameState.m_rocksLeft);
    m_points = std::move(a_gameState.m_points);
    m_board = std::move(a_gameState.m_board);
}

int GameState::getNumberOfPlayers() const noexcept
{
    return m_numberOfPlayers;
}

int GameState::getNumberOfHumanPayers() const noexcept
{
    return m_numberOfHumanPlayers;
}

int GameState::getNumberOfBotPlayers() const noexcept
{
    return m_numberOfBotPlayers;
}

int GameState::getHolesPerSide() const noexcept
{
    return m_holesPerSide;
}

const std::vector<int>& GameState::getPoints() const noexcept
{
    return m_points;
}

const std::vector<int>& GameState::getBoard() const noexcept
{
    return m_board;
}

const std::vector<int>& GameState::getRocksLeft() const noexcept
{
    return m_rocksLeft;
}

const std::vector<std::string>& GameState::getPlayerNames() const noexcept
{
    return m_playerNames;
}

void GameState::setPoints(int a_player, int a_newValue) noexcept
{
    m_points[a_player] = a_newValue;
}


