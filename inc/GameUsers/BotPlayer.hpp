//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_BOTPLAYER_HPP
#define LIST_8_BOTPLAYER_HPP

#include <vector>
#include "Player.hpp"

class Board;
class PlayerScore;
class BotPlayer : public Player
{
public:
    BotPlayer(const std::string& a_name, Board& a_board, std::vector<PlayerScore>& a_playersScores) noexcept;
private:
};

#endif //LIST_8_BOTPLAYER_HPP
