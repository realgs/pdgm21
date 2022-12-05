//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <iostream>

#include "HumanPlayer.hpp"

HumanPlayer::HumanPlayer(ISession& a_session, int a_playerID) noexcept : IPlayer(a_session, a_playerID)
{

}

int HumanPlayer::getMove() noexcept
{
    // TODO: doesnt make sense with client-server architecture
    int move;
    std::cin >> move;
    return move;
}



bool HumanPlayer::acceptMove(int a_move) noexcept
{
    std::cout << __func__ << ": called" << std::endl;
    return true;
}

bool HumanPlayer::declineMove(int a_move) noexcept
{
    std::cout << __func__ << ": called" << std::endl;
    return true;
}
