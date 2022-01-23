//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_SYSTEM_HPP
#define LIST_8_SYSTEM_HPP

#include <memory>
#include "Server.hpp"

class System
{
public:
    System() noexcept;

    void startGame() noexcept;

private:
    GameState m_gameSettings;
    std::unique_ptr<Server> m_gameServer;
};

#endif //LIST_8_SYSTEM_HPP
