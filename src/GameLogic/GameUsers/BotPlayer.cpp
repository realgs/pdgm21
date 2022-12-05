//
// Author: jakubszwedowicz
// Date: 2001.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include <string>


#include "BotPlayer.hpp"
#include "ISession.hpp"
#include "GameState.hpp"
#include "DecisionTree.hpp"

BotPlayer::BotPlayer(ISession& a_session, int a_playerID) noexcept
        : IPlayer(a_session, a_playerID)
        , m_decisionTree(a_playerID, 0, a_session.getCurrentGameState())
{

}

int BotPlayer::getMove() noexcept
{
    if(m_moves.empty())
    {
        buildNewMoves();
    }
    int move = m_moves.back();
    return move;
}

bool BotPlayer::acceptMove(int a_move) noexcept
{
    if (m_moves.back() != a_move)
    {
        // TODO: Since it comes validated from the server it cannot be wrong. Still though it has to be investigated later on.
        std::cout << "Something weird happened" << std::endl;
        return false;
    }
    m_moves.pop_back();
    return true;
}

bool BotPlayer::declineMove(int a_move) noexcept
{
    // Bot must have made a wrong move, maybe it's unsynchronized
    buildNewMoves();
    std::cout << "Bot move declined!" << std::endl;
    return true;
}

void BotPlayer::buildNewMoves() noexcept
{
    m_decisionTree.updateTreeWithMove(GameState(m_session.getCurrentGameState()));
    m_moves = m_decisionTree.getBestMoves();
}