//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <iostream>

#include "HumanPlayer.hpp"

HumanPlayer::HumanPlayer(const std::string& a_name, const GameState& a_gameState) noexcept : Player(a_name, a_gameState)
{

}

int HumanPlayer::makeTurn() noexcept
{
    int move;
    std::cin >> move;
    return move;
}
