//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <memory>

#include "LocalClient.hpp"
#include "IPublisher.hpp"
#include "BotPlayer.hpp"

LocalClient::LocalClient(IPublisher& a_server, const GameState& a_gameState, int a_clientID) noexcept
        : IClient(a_server), m_clientID(a_clientID), m_gameState(a_gameState)
        , m_player(std::make_unique<BotPlayer>(m_gameState.getPlayerNames()[a_clientID], m_gameState, a_clientID))
{

}


void LocalClient::updateGameState() noexcept
{
    // Does nothing since LocalClient runs on GameSession and is directly connected to GameSession resources via references
}

int LocalClient::makeTurn() const noexcept
{
    return m_player->makeTurn();
}
