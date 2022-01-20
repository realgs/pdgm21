//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <memory>
#include <optional>
#include "Server.hpp"
#include "Board.hpp"
#include "HumanPlayer.hpp"
#include "HumanClient.hpp"
#include "BotClient.hpp"

Server::Server() noexcept
        : m_settings(nullptr), m_settingsChanged(false), m_playersScores(), m_clients(), m_board(nullptr)
{

}

void Server::run(Settings* a_settings) noexcept
{
    prepareGame(a_settings);
    runGame();
}

void Server::runGame() noexcept
{
    for (int i = 0; i < m_settings->m_numbersInfo.m_numberOfPlayers.second; i++)
    {
//        if(m_settings->m_players[i].getPlayerType() == PlayerType::human)
        std::cout << "Turn of player: " << m_settings->m_players[i].getName() << std::endl;
        int move = m_clients[i]->makeTurn();
        if (validateMove(move, i))
        {
            m_board->unloadHole(move);
        } else
        {
            std::cout << "You tried making illegal move!" << std::endl;
            i--;
        }

    }
}

bool Server::validateMove(int a_move, int a_playerIndex) const noexcept
{
    return a_move >= m_board->validateMove(a_move, a_playerIndex);
}

// PRIVATE
void Server::prepareGame(Settings* a_settings) noexcept
{
    changeSettings(a_settings);
    applySettings();
}

void Server::changeSettings(Settings* a_settings) noexcept
{
    if (m_settings == nullptr || a_settings->getSettingsID() != m_settings->getSettingsID())
    {
        m_settings = a_settings;
        m_settingsChanged = true;
    }
}

void Server::applySettings() noexcept
{
    if (m_settingsChanged)
    {
        init();
        synchronizeClients();
        m_settingsChanged = false;
    }
    m_board->init(m_clients);
}

void Server::init() noexcept
{
    m_settings->init();
    m_playersScores.clear();
    for (int i = 0; i < m_settings->m_numbersInfo.m_numberOfPlayers.second; i++)
        m_playersScores.push_back(0);

    m_board = std::make_unique<Board>(m_settings->m_numbersInfo.m_numberOfPlayers.second,
                                      m_settings->m_numbersInfo.m_numberOfHousesPerSide.second,
                                      m_settings->m_numbersInfo.m_numberOfRocksPerHouse.second);

    for (int i = 0; i < m_settings->m_numbersInfo.m_numberOfHumanPlayers.second; i++)
    {
        m_clients.push_back(std::make_unique<HumanClient>(m_settings->m_players[i], m_playersScores, *this));
    }
    for (int i = 0; i < m_settings->m_numbersInfo.m_numberOfSiPlayers.second; i++)
    {
        m_clients.push_back(std::make_unique<BotClient>(m_settings->m_players[i], m_playersScores, *m_board, *this));
    }
}

std::optional<Board> Server::getBoard(PlayerSettings a_playerSetting) const noexcept
{
    return (verifyPlayerSettings(a_playerSetting)) ? std::make_optional(Board(*m_board)) : std::nullopt;
}

std::optional<std::vector<PlayerScore>> Server::getPlayersScore(PlayerSettings a_playerSetting) const noexcept
{
    return verifyPlayerSettings(a_playerSetting) ? std::make_optional(
            m_playersScores) : std::nullopt;
}

bool Server::verifyPlayerSettings(const PlayerSettings& a_playerSettings) const noexcept
{
    return (m_settings->m_players.size() > a_playerSettings.getPlayerID())
           && (m_settings->m_players[a_playerSettings.getPlayerID()].getName() == a_playerSettings.getName());
}




