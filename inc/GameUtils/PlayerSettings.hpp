//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_PLAYERSETTINGS_HPP
#define LIST_8_PLAYERSETTINGS_HPP
#include "PlayerType.hpp"

class PlayerSettings
{
public:
    PlayerSettings(PlayerType a_playerType, int a_playerID, const std::string& a_name) noexcept;
    const std::string& getName() const noexcept;
    int getPlayerID() const noexcept;
    PlayerType getPlayerType() const noexcept;
    void setName(const std::string& a_name) noexcept;
private:
    PlayerType m_playerType;
    int m_playerID;
    std::string m_name;
};
#endif //LIST_8_PLAYERSETTINGS_HPP
