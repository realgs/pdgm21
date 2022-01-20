//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include "IPublisher.hpp"

bool IPublisher::subscribe(ISubscriber& a_subscriber) noexcept
{
    m_subscribers.push_back(&a_subscriber);
    return true;
}

void IPublisher::synchronizeClients()
{
    for(auto& subscriber : m_subscribers)
    {
        subscriber->updateBoard();
        subscriber->updatePlayer();
    }
}
