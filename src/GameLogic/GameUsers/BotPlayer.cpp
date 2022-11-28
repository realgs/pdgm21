//
// Author: jakubszwedowicz
// Date: 2001.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include "BotPlayer.hpp"
#include "GameState.hpp"
#include "DecisionTree.hpp"

BotPlayer::BotPlayer(const std::string& a_name, const GameState& a_gameState, int a_playerID) noexcept
        : Player(a_name, a_gameState)
        , m_decisionTree(a_playerID, 0, m_gameState)
{

}

int BotPlayer::makeTurn() noexcept
{
    if(m_moves.empty())
    {
        m_decisionTree.updateTreeWithMove(GameState(m_gameState));
        m_moves = m_decisionTree.getBestMoves();
    }
    int move = m_moves.back();
    m_moves.pop_back();
    return move;
}
