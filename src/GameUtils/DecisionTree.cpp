//
// Author: jakubszwedowicz
// Date: 23.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <list>
#include <tuple>

#include "DecisionTree.hpp"

DecisionTree::DecisionTree(int a_decisionTreeOwner, int a_previouslyMadeMove, const GameState& a_gameState) noexcept
        : DecisionTree(a_decisionTreeOwner, a_previouslyMadeMove, GameState(a_gameState))
{

}

DecisionTree::DecisionTree(int a_decisionTreeOwner, int a_previouslyMadeMove, GameState&& a_gameState) noexcept
        : m_decisionTreeOwner(a_decisionTreeOwner)
        , m_previouslyMadeMove(a_previouslyMadeMove)
        , m_gameState(std::move(a_gameState))
{
    countFitness();
}

void DecisionTree::updateTreeWithMove(GameState&& a_newGameState) noexcept
{
    m_gameState.updateRocksPointsBoardWithOther(std::forward<GameState&&>(a_newGameState));
    countFitness();
    expandTree();
}

std::vector<int> DecisionTree::getBestMoves() const noexcept
{
    std::vector<std::pair<int, int>> bestMoveFitness = {{0, -999999}};
    std::vector<std::pair<int, int>> currMoveFitness;
    getBestMoves(bestMoveFitness, currMoveFitness);
    std::vector<int> res;
    std::for_each(std::crbegin(bestMoveFitness), std::crend(bestMoveFitness),
                  [&res](const std::pair<int, int>& moveFitness)
                  {
                      res.push_back(moveFitness.first);
                  });
    return res;
}

void DecisionTree::getBestMoves(std::vector<std::pair<int, int>>& a_currBestMoveFitness,
                                std::vector<std::pair<int, int>>& a_currMoveFitness) const noexcept
{
    if (m_moves.empty())
    {
        if (m_fitness > a_currBestMoveFitness.back().second)
        {
            a_currBestMoveFitness = a_currMoveFitness;
        }
    } else
    {
        for (auto& nextMove: m_moves)
        {
            a_currMoveFitness.push_back({nextMove.m_previouslyMadeMove, nextMove.m_fitness});
            nextMove.getBestMoves(a_currBestMoveFitness, a_currMoveFitness);
        }
    }
    a_currMoveFitness.pop_back();
    return;
}

void DecisionTree::expandTree() noexcept
{
    m_moves.clear();
    int firstHouseOfOwner = m_decisionTreeOwner * m_gameState.getHolesPerSide();    // Inclusive
    int lastHouseOfOwner = (m_decisionTreeOwner + 1) * m_gameState.getHolesPerSide() - 2;   // Inclusive
    for (int i = firstHouseOfOwner, move = 0; i <= lastHouseOfOwner; i++)
    {
        if (!m_gameState.validateMove(i, m_decisionTreeOwner)) continue;

        GameState nextGameState(m_gameState);
        int lastHolePutInto = nextGameState.unloadHouse(i, m_decisionTreeOwner);
        m_moves.push_back(std::move(DecisionTree(m_decisionTreeOwner, i, std::move(nextGameState))));   // Copy elision
        if (lastHolePutInto == (m_decisionTreeOwner + 1) * m_gameState.getHolesPerSide() - 1) // Extra move
        {
            m_moves[move].expandTree();
        }
        move++;
    }
}

void DecisionTree::countFitness() noexcept
{
    int opponentIndex = (m_decisionTreeOwner + 1) % m_gameState.getNumberOfPlayers();
    const auto& points = m_gameState.getPoints();

    m_fitness = points[m_decisionTreeOwner] - points[opponentIndex];

    if(m_gameState.getRocksLeft()[m_decisionTreeOwner] == 0)
    {
        m_fitness += m_gameState.getRocksLeft()[opponentIndex] / 2;
    }
}
