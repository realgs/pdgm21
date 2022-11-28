//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <memory>
#include <optional>
#include <future>
#include <exception>

#include "GameSession.hpp"
#include "IClient.hpp"
#include "HumanPlayer.hpp"
#include "RemoteClient.hpp"
#include "LocalClient.hpp"

GameSession::GameSession(const GameState& a_startingGameState) noexcept
        : m_startingGameState(a_startingGameState), m_currentGameState(m_startingGameState), m_clients()
{

}

void GameSession::run() noexcept
{
    prepareGame();
    runGame();
}

// PRIVATE
void GameSession::prepareGame() noexcept
{
    m_clients.clear();
    m_currentGameState = m_startingGameState;
    for (int i = 0; i < m_currentGameState.getNumberOfHumanPayers(); i++)
        m_clients.push_back(std::make_unique<RemoteClient>(*this, i));

    for (int i = m_currentGameState.getNumberOfHumanPayers(); i < m_currentGameState.getNumberOfPlayers(); i++)
        m_clients.push_back(std::make_unique<LocalClient>(*this, m_currentGameState, i));

    synchronizeClients();
}

void GameSession::runGame() noexcept
{
    int turnCounter = 1;
    bool running = true;

    try
    {
        while (running)
        {
            for (int i = 0; i < m_currentGameState.getNumberOfPlayers(); i++, turnCounter++)
            {
                std::cout << "Turn " << turnCounter << ": " << m_currentGameState.getPlayerNames()[i] << std::endl;
                std::cout << "Rocks left: " << m_currentGameState.getRocksLeft()[i] << std::endl;
                std::cout << "Points: " << m_currentGameState.getPoints()[i] << std::endl;
                std::cout << m_currentGameState << std::endl;

                if (m_currentGameState.getRocksLeft()[i] == 0)  // GameSession ends if moving player has no more rocks to play
                {
                    std::cout << "Player: " << m_currentGameState.getPlayerNames()[i]
                              << " has no more moves. GameSession ends." << std::endl;
                    running = false;
                    m_currentGameState.finishGame(i);
                    break;
                }

                int lastHolePutInto = oneValidTurn(i);

                if (lastHolePutInto == (i + 1) * m_currentGameState.getHolesPerSide() - 1) // Extra move
                {
                    i--;
                }
                synchronizeClients();
            }
        }
        annouceTheWinner();
    } catch (const std::exception& ex)
    {
        std::cout << "Something went terribly wrong! \n" << ex.what() << std::endl;
    }
}


GameState GameSession::getCurrentGameState() const noexcept
{
    return m_currentGameState;
}

void GameSession::annouceTheWinner() const noexcept
{
    const auto& points = m_currentGameState.getPoints();
    auto winnerPoints = std::max_element(points.cbegin(), points.cend());
    int winnerIndex = std::distance(points.cbegin(), winnerPoints);
    std::cout << m_currentGameState.getPlayerNames()[winnerIndex] << " won with " << *winnerPoints << std::endl;
}

int GameSession::oneValidTurn(int a_playerIndex)
{
    bool validMoveWasMade = false;
    int patience = 3;
    int lastHouse = -1;
    while (!validMoveWasMade && patience != 0)
    {
        std::future<int> clientThread = std::async(std::launch::async, &IClient::makeTurn,
                                                   m_clients[a_playerIndex].get());

        std::future_status status = std::future_status::deferred;
        while (status != std::future_status::ready && patience != 0)
        {
            using namespace std::chrono_literals;
            switch (status = clientThread.wait_for(30s); status)
            {
                case std::future_status::ready:
                {
                    int move = clientThread.get();
                    if (m_currentGameState.validateMove(move, a_playerIndex))
                    {
                        lastHouse = m_currentGameState.unloadHouse(move, a_playerIndex);
                        validMoveWasMade = true;
                    } else
                    {
                        std::cout << "You tried making illegal move!"
                                     "\nChoose again." << std::endl;
                    }
                    break;
                }
                case std::future_status::timeout:
                    std::cout << "30 seconds have passed, you have to make a decision!" << std::endl;
                    patience--;
                    break;
                case std::future_status::deferred:
                    throw std::runtime_error("Deferred state of clientThread!");
            }
        }
    }
    if (patience == 0)
    {
        std::cout << "You didn't make any decision. Your turn is skipped." << std::endl;
    }
    return lastHouse;
}
