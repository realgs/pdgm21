//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include "Client.hpp"

Client::Client(IPublisher& a_publisher) noexcept : ISubscriber(a_publisher)
{

}

Player* Client::getPlayer() const noexcept
{
    return m_player.get();
}
