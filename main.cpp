#include <iostream>
#include "Board.hpp"
#include "Player.hpp"
#include "HumanPlayer.hpp"
#include "inc/GameSystems/System.hpp"

int main()
{
    // Project doesn't work. I broke it with the change of Board::m_holes type from;
    // std::vector<Hole>
    // to
    // std::vector<std::unique_ptr<Hole>>.
    // It broke the entire Observer pattern and server-client solution with server giving a copy of board (and others) to client.
    System sys;
    sys.startGame();
}
