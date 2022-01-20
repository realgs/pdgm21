//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_HUMANPLAYER_HPP
#define LIST_8_HUMANPLAYER_HPP
#include "Player.hpp"
#include "PlayerType.hpp"
#include "Client.hpp"

class HumanPlayer : public Player
{
public:
    HumanPlayer(const std::string& a_name, Board& a_board, std::vector<PlayerScore>& a_playersScores) noexcept;
private:
};

#endif //LIST_8_HUMANPLAYER_HPP
