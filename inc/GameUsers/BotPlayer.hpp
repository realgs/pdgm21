//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_BOTPLAYER_HPP
#define LIST_8_BOTPLAYER_HPP

#include "Player.hpp"
#include "DecisionTree.hpp"

class GameState;
class BotPlayer : public Player
{
public:
    BotPlayer(const std::string& a_name, const GameState& a_gameState, int a_playerID) noexcept;
    ~BotPlayer() override = default;

    int makeTurn() noexcept override;

private:
    DecisionTree m_decisionTree;
    std::vector<int> m_moves;
};

#endif //LIST_8_BOTPLAYER_HPP
