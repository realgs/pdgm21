//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <iostream>
#include <vector>

#include "Player.hpp"

Player::Player(const std::string& a_name, const GameState& a_gameState) noexcept : m_name(a_name), m_gameState(a_gameState)
{

}

