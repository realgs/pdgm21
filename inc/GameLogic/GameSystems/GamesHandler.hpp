//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_GAMESHANDLER_HPP
#define LIST_8_GAMESHANDLER_HPP

#include <vector>
#include <memory>
#include "GameState.hpp"

// TODO: Can I forward declare it? I guess no
#include "ILocalSession.hpp"
#include "IRemoteSession.hpp"
#include "ISession.hpp"

class GamesHandler
{
public:
    GamesHandler() noexcept;

    void startSystem() noexcept;

private:
    bool verify_move;
    GameState m_gameSettings;
    std::vector<IRemoteSession> m_remoteGameSessions;
    std::vector<ILocalSession> m_localGameSessions;
    std::vector<ISession> m_gameSessions;
};

#endif //LIST_8_GAMESHANDLER_HPP
