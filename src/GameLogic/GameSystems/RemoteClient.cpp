//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <memory>

#include "RemoteClient.hpp"
#include "HumanPlayer.hpp"
#include "GameSession.hpp"

RemoteClient::RemoteClient(IPublisher& a_server, int a_clientID) noexcept
        : IClient(a_server), m_clientID(a_clientID), m_gameState(a_server.getCurrentGameState()), m_player(
        std::make_unique<HumanPlayer>(m_gameState.getPlayerNames()[a_clientID], m_gameState))
{
}


void RemoteClient::updateGameState() noexcept
{
    m_gameState = m_publisher.getCurrentGameState();
}


int RemoteClient::makeTurn() const noexcept
{
    return m_player->makeTurn();
}
