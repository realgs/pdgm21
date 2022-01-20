//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <vector>
#include <iostream>

#include "Player.hpp"
#include "Board.hpp"

Player::Player(const std::string& a_name, Board& a_board, std::vector<PlayerScore>& a_playersScores) noexcept : m_name(a_name), m_board(a_board), m_playersScores(a_playersScores)
{

}

int Player::makeTurn() noexcept
{
    int move;
    std::cin >> move;
    return move;
}

void Player::printBoard() const noexcept
{
    std::cout << "Turn of player: " << m_name << std::endl;
    std::cout << "Player 0\nScore = " << m_playersScores[0].getScore() << "\n" << std::endl;
    std::cout << m_board << std::endl;
    std::cout << "Player 1\nScore = " << m_playersScores[1].getScore() << "\n" << std::endl;
}
