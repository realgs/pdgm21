//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_HUMANPLAYER_HPP
#define LIST_8_HUMANPLAYER_HPP

#include "Player.hpp"

class HumanPlayer : public Player
{
public:
    HumanPlayer(const std::string& a_name, const GameState& a_gameState) noexcept;

    int makeTurn() noexcept override;

private:
};

#endif //LIST_8_HUMANPLAYER_HPP
