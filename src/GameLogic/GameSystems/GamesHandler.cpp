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
    if (m_gameServer == nullptr)
        m_gameServer = std::make_unique<GameSession>(m_gameSettings);
    m_gameServer->run();
}



