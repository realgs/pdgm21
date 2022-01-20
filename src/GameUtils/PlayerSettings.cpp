//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <string>

#include "PlayerType.hpp"
#include "PlayerSettings.hpp"

PlayerSettings::PlayerSettings(PlayerType a_playerType, int a_playerID, const std::string& a_name) noexcept : m_playerType(a_playerType), m_playerID(a_playerID), m_name(a_name)
{

}

const std::string& PlayerSettings::getName() const noexcept
{
    return m_name;
}

int PlayerSettings::getPlayerID() const noexcept
{
    return 0;
}

PlayerType PlayerSettings::getPlayerType() const noexcept
{
    return m_playerType;
}

void PlayerSettings::setName(const std::string& a_name) noexcept
{
    m_name = a_name;
}


