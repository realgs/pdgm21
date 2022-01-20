//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <iostream>
#include "System.hpp"

System::System() noexcept
{
}

void System::changeSettings() noexcept
{
    m_settings.changeSetting();
}

void System::startGame() noexcept
{
    if (m_settings.areOk())
    {
        if(m_gameServer == nullptr) m_gameServer = std::make_unique<Server>();
        m_gameServer->run(&m_settings);
    }

}


