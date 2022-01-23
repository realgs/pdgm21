//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_PLAYERCLIENT_HPP
#define LIST_8_PLAYERCLIENT_HPP

#include "memory"
#include "ISubscriber.hpp"
#include "Player.hpp"

class HumanClient : public ISubscriber
{
public:
    HumanClient(IPublisher& a_publisher, int a_clientID) noexcept;
    HumanClient(const HumanClient&) = delete;
    HumanClient(HumanClient*&) = delete;

    int makeTurn() const noexcept override;

    void updateGameState() noexcept override;

private:
    int m_clientID;
    GameState m_gameState;
    std::unique_ptr<Player> m_player;
};

#endif //LIST_8_PLAYERCLIENT_HPP
