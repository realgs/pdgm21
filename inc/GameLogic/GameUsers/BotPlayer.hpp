//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_BOTPLAYER_HPP
#define LIST_8_BOTPLAYER_HPP

#include "IPlayer.hpp"
#include "DecisionTree.hpp"

class GameState;
class BotPlayer : public IPlayer
{
public:
    BotPlayer(ISession& a_session, int a_playerID) noexcept;
    ~BotPlayer() override = default;

    int getMove() noexcept override;
    bool acceptMove(int a_move) noexcept override;
    bool declineMove(int a_move) noexcept override;

private:
    void buildNewMoves() noexcept;

    DecisionTree m_decisionTree;
    std::vector<int> m_moves;
};

#endif //LIST_8_BOTPLAYER_HPP
