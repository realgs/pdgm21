//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_ISUBSCRIBER_HPP
#define LIST_8_ISUBSCRIBER_HPP

#include "GameState.hpp"

class IPublisher;
class ISubscriber
{
public:
    ISubscriber(IPublisher& a_publisher) noexcept;
    virtual ~ISubscriber() = default;
    virtual void updateGameState() noexcept = 0;
    virtual int makeTurn() const noexcept = 0;
protected:
    IPublisher& m_publisher;
};
#endif //LIST_8_ISUBSCRIBER_HPP
