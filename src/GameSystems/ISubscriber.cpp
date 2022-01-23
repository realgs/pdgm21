//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include "ISubscriber.hpp"
#include "IPublisher.hpp"

ISubscriber::ISubscriber(IPublisher& a_publisher) noexcept : m_publisher(a_publisher)
{
    a_publisher.subscribe(*this);
}

