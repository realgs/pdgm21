//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include "GamesHandler.hpp"
#include "GameState.hpp"


//GamesHandler::GamesHandler() noexcept : m_gameSettings({"Player1", "Bot1"})
GamesHandler::GamesHandler() noexcept : m_gameSettings({"Bot1", "Bot2"}, 0, 2)
{

}

void GamesHandler::startSystem() noexcept
{
    std::vector<IRemoteSession> m_remoteGameSessions;
    std::vector<ILocalSession> m_localGameSessions;
    std::vector<ISession> m_gameSessions;
    if (m_localGameSessions == nullptr)
        m_gameServer = std::make_unique<GameSession>(m_gameSettings);
    m_gameServer->run();
}



