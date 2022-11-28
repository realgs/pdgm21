//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_LOCALCLIENT_HPP
#define LIST_8_LOCALCLIENT_HPP

#include <memory>

#include "IClient.hpp"
#include "Player.hpp"
#include "DecisionTree.hpp"

class LocalClient : public IClient
{
public:
    LocalClient(IPublisher& a_publisher, const GameState& a_gameState, int a_clientID) noexcept;

    int makeTurn() const noexcept override;

    virtual void updateGameState() noexcept override;

private:
    int m_clientID;
    const GameState& m_gameState;
    std::unique_ptr<Player> m_player;
};
#endif //LIST_8_LOCALCLIENT_HPP