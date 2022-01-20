//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com
//

#ifndef LIST_8_SETTINGS_HPP
#define LIST_8_SETTINGS_HPP

#include <iostream>
#include <array>
#include <vector>
#include "PlayerSettings.hpp"

class Player;

struct Settings
{
    std::ostream& operator<<(std::ostream& a_out) const;
//        friend std::ostream& operator<<(std::ostream& a_out, const Settings& a_settings);

    bool areOk() const noexcept;

    bool changeSetting() noexcept;

    void init() noexcept;

    int getSettingsID() const noexcept;

    struct NumbersInfo
    {
        static constexpr int m_numberOfNumberSettings = 8;

        std::pair<std::string, int> m_numberOfHumanPlayers = {"Numbers of human players", 1};
        std::pair<std::string, int> m_numberOfSiPlayers = {"Numbers of Si players", 1};
        std::pair<std::string, int> m_numberOfRocksPerHouse = {"Numbers of Rocks per house", 4};
        std::pair<std::string, int> m_numberOfHousesPerSide = {"Numbers of houses per side", 6};

        std::pair<std::string, int> m_numberOfPlayers = {"Total number of players", m_numberOfHumanPlayers.second +
                                                                                    m_numberOfSiPlayers.second};
        std::pair<std::string, int> m_numberOfRocksInGame = {"Total number of Rocks in game",
                                                             m_numberOfRocksPerHouse.second *
                                                             m_numberOfHousesPerSide.second *
                                                             m_numberOfPlayers.second};
        std::pair<std::string, int> m_numberOfHolesPerSide = {"Total number of holes (houses + malakans) per side",
                                                              m_numberOfHousesPerSide.second + 1};
        std::pair<std::string, int> m_numberOfHolesOnBoard = {"Total number of holes on board",
                                                              m_numberOfHolesPerSide.second *
                                                              m_numberOfPlayers.second};

        std::array<std::pair<std::string, int>*, m_numberOfNumberSettings> m_settings
                {
                        &m_numberOfHumanPlayers, &m_numberOfSiPlayers, &m_numberOfRocksPerHouse,
                        &m_numberOfHousesPerSide,
                        &m_numberOfPlayers, &m_numberOfRocksInGame, &m_numberOfHolesPerSide, &m_numberOfHolesOnBoard
                };
    };
//private:  // Bad
    static int s_settingsID;
    int m_settingsID = 0;
    std::vector<PlayerSettings> m_players;
    NumbersInfo m_numbersInfo;
};


#endif //LIST_8_SETTINGS_HPP
