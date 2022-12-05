//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_HUMANPLAYER_HPP
#define LIST_8_HUMANPLAYER_HPP

#include <string>

#include "IPlayer.hpp"

class HumanPlayer : public IPlayer {
public:
    HumanPlayer(ISession &a_session, int a_playerID) noexcept;

    int getMove() noexcept override;

    bool acceptMove(int a_move) noexcept;

    bool declineMove(int a_move) noexcept;

private:
};

#endif //LIST_8_HUMANPLAYER_HPP
