//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_ICLIENT_HPP
#define LIST_8_ICLIENT_HPP

#include "GameState.hpp"

class IPublisher;
class IClient
{
public:
    IClient(IPublisher& a_publisher) noexcept;
    virtual ~IClient() = default;
    virtual void updateGameState() noexcept = 0;
    virtual int makeTurn() const noexcept = 0;
protected:
    IPublisher& m_publisher;
};
#endif //LIST_8_ICLIENT_HPP
