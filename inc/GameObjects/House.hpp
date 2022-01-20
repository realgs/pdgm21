//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_HOUSE_HPP
#define LIST_8_HOUSE_HPP

#include "Hole.hpp"

class Client;
class House : public Hole
{
public:
    House(Client* a_owner) noexcept;
    House(const House& a_other) noexcept;
private:

};

#endif //LIST_8_HOUSE_HPP
