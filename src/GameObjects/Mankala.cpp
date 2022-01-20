//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include <string>
#include <exception>

#include "Mankala.hpp"
#include "Client.hpp"

Mankala::Mankala(Client* a_owner) noexcept : Hole(a_owner)
{

}

bool Mankala::addRock(Rock& a_rock)
{
    if(a_rock.getOwner() != m_owner)
    {
        return false;
    }
    m_rocks.push_back(&a_rock);
    return true;
}
