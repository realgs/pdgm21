//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include "Rock.hpp"
#include "Client.hpp"

Rock::Rock(Client* a_owner) noexcept : m_owner(a_owner)
{

}

Client* Rock::getOwner() const noexcept
{
    return m_owner;
}

void Rock::setOwner(Client* a_newOwner) noexcept
{
    m_owner = a_newOwner;
}
