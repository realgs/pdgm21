//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <memory>

#include "BotClient.hpp"
#include "IPublisher.hpp"
#include "BotPlayer.hpp"

BotClient::BotClient(IPublisher& a_server, const GameState& a_gameState, int a_clientID) noexcept
        : ISubscriber(a_server), m_clientID(a_clientID), m_gameState(a_gameState)
        , m_player(std::make_unique<BotPlayer>(m_gameState.getPlayerNames()[a_clientID], m_gameState, a_clientID))
{

}


void BotClient::updateGameState() noexcept
{
    // Does nothing since BotClient runs on Server and is directly connected to Server resources via references
}

int BotClient::makeTurn() const noexcept
{
    return m_player->makeTurn();
}
