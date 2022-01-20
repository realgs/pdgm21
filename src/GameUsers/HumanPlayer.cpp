//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include "HumanPlayer.hpp"
#include "Client.hpp"
#include "Board.hpp"

HumanPlayer::HumanPlayer(const std::string& a_name, Board& a_board, std::vector<PlayerScore>& a_playersScores) noexcept : Player(a_name, a_board, a_playersScores)

{

}
