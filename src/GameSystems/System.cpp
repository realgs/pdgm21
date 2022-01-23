//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include "System.hpp"
#include "GameState.hpp"


//System::System() noexcept : m_gameSettings({"Player1", "Bot1"})
System::System() noexcept : m_gameSettings({"Bot1", "Bot2"}, 0, 2)
{

}

void System::startGame() noexcept
{
    if (m_gameServer == nullptr)
        m_gameServer = std::make_unique<Server>(m_gameSettings);
    m_gameServer->run();
}



