//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_HOLE_HPP
#define LIST_8_HOLE_HPP

#include <vector>
#include <memory>
#include <stack>
#include "Rock.hpp"

class Client;
class Hole
{
public:
    // CTORs
    Hole(Client* owner, const std::vector<Rock*>& a_startingRocks = {}) noexcept;

    virtual ~Hole() noexcept = default;

    // FUNCTIONS
    void addManyRocks(const std::vector<Rock*>& a_manyRocks) noexcept;

    virtual bool addRock(Rock& a_rock);

    Rock& removeRock() noexcept;

    std::vector<Rock*> takeRocks() noexcept;

    void clearHole() noexcept;

    // GETTERS
    int getRocksNumber() const noexcept;

    Client* getOwner() const noexcept;

    // SETTERS
protected:
    Client* m_owner;
    std::vector<Rock*> m_rocks;
};

#endif //LIST_8_HOLE_HPP
