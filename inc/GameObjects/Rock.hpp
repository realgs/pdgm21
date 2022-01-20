//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_ROCK_HPP
#define LIST_8_ROCK_HPP

class Client;
class Rock
{
public:
    // CTORs
    Rock(Client* a_owner) noexcept;

    // GETTERS
    Client* getOwner() const noexcept;

    // SETTERS
    void setOwner(Client* a_newOwner) noexcept;


private:
    Client* m_owner;


};

#endif //LIST_8_ROCK_HPP
