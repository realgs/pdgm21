//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_PLAYER_HPP
#define LIST_8_PLAYER_HPP

#include <vector>
#include <string>

#include "PlayerType.hpp"
#include "PlayerScore.hpp"

class Board;

class PlayerScore;

class Player
{
public:
    Player(const std::string& a_name, Board& a_board, std::vector<PlayerScore>& a_playersScores) noexcept;
    int makeTurn() noexcept;
    virtual ~Player() = default;
private:
    void printBoard() const noexcept;
    const std::string& m_name;
    Board& m_board;
    std::vector<PlayerScore>& m_playersScores;
};

#endif //LIST_8_PLAYER_HPP
