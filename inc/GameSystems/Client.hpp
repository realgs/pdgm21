//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_CLIENT_HPP
#define LIST_8_CLIENT_HPP

#include <memory>
#include "ISubscriber.hpp"
#include "../GameUtils/PlayerSettings.hpp"
#include "PlayerScore.hpp"
#include "Player.hpp"
#include "Board.hpp"

class Client : public ISubscriber
{
public:
    Client(IPublisher& a_publisher) noexcept;
    virtual ~Client() = default;
    Player* getPlayer() const noexcept;
    virtual int makeTurn() noexcept = 0;
//    virtual const Board& getBoard() const noexcept = 0;
//    virtual const PlayerScore& getPlayerScore() const noexcept = 0;
protected:
    std::unique_ptr<Player> m_player;
};
#endif //LIST_8_CLIENT_HPP
