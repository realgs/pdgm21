//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include "Settings.hpp"
#include "PlayerType.hpp"

int Settings::s_settingsID = 0;

std::ostream& Settings::operator<<(std::ostream& a_out) const
{
    a_out << m_numbersInfo.m_settings[0]->first;
    for (int i = 1; i < Settings::NumbersInfo::m_numberOfNumberSettings; i++)
    {
        a_out << "\n" << m_numbersInfo.m_settings[i]->first;
    }
    return a_out;
}

bool Settings::areOk() const noexcept
{
    return m_numbersInfo.m_numberOfRocksInGame.second > 0;
}

bool Settings::changeSetting() noexcept
{
    int option = -1;
    while (option != 0)
    {
        std::cout << "Choose which settings to change: " << std::endl;
        std::cout << "0 - Return to Menu" << std::endl;
        for (int i = 1; i < Settings::NumbersInfo::m_numberOfNumberSettings; i++)
            std::cout << i << " - " << m_numbersInfo.m_settings[i]->first << std::endl;
        std::cin >> option;
        if (option != 0)
        {
            std::cout << "choose how you want to change the value: " << std::endl;
            int value;
            std::cin >> value;
            m_settingsID = ++s_settingsID;
            switch (option)
            {
                m_numbersInfo.m_settings[option]->second = value;
                // NOT FINISHED !!!!!!!!!!!!!!!!!1
            }
        }
    }
    return true;
}

void Settings::init() noexcept
{
    for(int i = 0; i < m_numbersInfo.m_numberOfHumanPlayers.second; i++)
    {
        m_players.push_back(PlayerSettings(PlayerType::human, i, "Human" + std::to_string(i)));
    }
    for(int i = m_numbersInfo.m_numberOfHumanPlayers.second; i < m_numbersInfo.m_numberOfPlayers.second; i++)
    {
        m_players.push_back(PlayerSettings(PlayerType::easySI, i, "Bot" + std::to_string(i)));
    }
}

int Settings::getSettingsID() const noexcept
{
    return m_settingsID;
}




