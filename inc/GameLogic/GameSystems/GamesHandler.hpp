//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_GAMESHANDLER_HPP
#define LIST_8_GAMESHANDLER_HPP

#include <memory>
#include "GameSession.hpp"

class GamesHandler
{
public:
    GamesHandler() noexcept;

    void startSystem() noexcept;

private:
    GameState m_gameSettings;
    std::unique_ptr<GameSession> m_gameServer;
};

#endif //LIST_8_GAMESHANDLER_HPP
