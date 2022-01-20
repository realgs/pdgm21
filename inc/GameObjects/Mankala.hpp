//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_MANKALA_HPP
#define LIST_8_MANKALA_HPP

#include "Hole.hpp"

class Client;
class Mankala : public Hole
{
public:
    // CTORs
    Mankala(Client* a_owner) noexcept;

    ~Mankala() noexcept = default;
    // FUNCTIONS

    bool addRock(Rock& a_rock);

    // GETTERS

    // SETTERS
private:
    Client* m_owner;

};

#endif //LIST_8_MANKALA_HPP