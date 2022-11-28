//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include "IClient.hpp"
#include "IPublisher.hpp"

IClient::IClient(IPublisher& a_publisher) noexcept : m_publisher(a_publisher)
{
    a_publisher.subscribe(*this);
}

