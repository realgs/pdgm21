//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_SYSTEM_HPP
#define LIST_8_SYSTEM_HPP

#include "Server.hpp"


class System
{
public:
    System() noexcept;

    void changeSettings() noexcept;

    void startGame() noexcept;

private:
    std::unique_ptr<Server> m_gameServer;
    Settings m_settings;
};

#endif //LIST_8_SYSTEM_HPP
