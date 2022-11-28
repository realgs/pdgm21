//
// Author: jakubszwedowicz
// Date: 23.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_DECISIONTREE_HPP
#define LIST_8_DECISIONTREE_HPP

#include <list>

#include "GameState.hpp"

class DecisionTree
{
public:
    DecisionTree(int a_decisionTreeOwner, int a_previouslyMadeMove, const GameState& a_gameState) noexcept;
    void updateTreeWithMove(GameState&& a_newGameState) noexcept;
    std::vector<int> getBestMoves() const noexcept;
private:
    DecisionTree(int a_decisionTreeOwner, int a_previouslyMadeMove, GameState&& a_gameState) noexcept;
    void getBestMoves(std::vector<std::pair<int, int>>& a_currBestMoveFitness, std::vector<std::pair<int, int>>& a_currMoveFitness) const noexcept;
    void expandTree() noexcept;
    void countFitness() noexcept;

    const int m_decisionTreeOwner;
    const int m_previouslyMadeMove;
    int m_fitness;
    GameState m_gameState;
    std::vector<DecisionTree> m_moves;

};
#endif //LIST_8_DECISIONTREE_HPP
