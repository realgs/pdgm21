//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include "Hole.hpp"
#include "Client.hpp"
#include "Mankala.hpp"

Hole::Hole(Client* a_owner, const std::vector<Rock*>& a_startingRocks) noexcept : m_owner(a_owner), m_rocks(a_startingRocks)
{

}

void Hole::addManyRocks(const std::vector<Rock*>& a_manyRocks) noexcept
{
    m_rocks.insert(m_rocks.end(), std::begin(a_manyRocks), std::end(a_manyRocks));
}

bool Hole::addRock(Rock& a_rock)
{
    a_rock.setOwner(m_owner);
    m_rocks.push_back(&a_rock);
    return true;
}
Rock& Hole::removeRock() noexcept
{
    Rock& last = *m_rocks.back();
    m_rocks.pop_back();
    return last;
}

std::vector<Rock*> Hole::takeRocks() noexcept
{
    std::vector<Rock*> res = m_rocks;
    m_rocks.clear();
    return res;
}

void Hole::clearHole() noexcept
{
    m_rocks.clear();
}

int Hole::getRocksNumber() const noexcept
{
    return m_rocks.size();
}

Client* Hole::getOwner() const noexcept
{
    return m_owner;
}

