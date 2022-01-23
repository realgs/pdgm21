//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_PLAYER_HPP
#define LIST_8_PLAYER_HPP

#include <string>

class PlayerScore;
class GameState;

class Player
{
public:
    Player(const std::string& a_name, const GameState& a_gameState) noexcept;
    virtual ~Player() = default;
    virtual int makeTurn() noexcept = 0;
protected:
    const std::string& m_name;
    const GameState& m_gameState;
private:
};

#endif //LIST_8_PLAYER_HPP
