//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_IPUBLISHER_HPP
#define LIST_8_IPUBLISHER_HPP

#include <vector>

#include "GameState.hpp"

class IClient;
class IPublisher
{
public:
    virtual ~IPublisher() = default;
    bool subscribe(IClient& a_subscriber) noexcept;
    virtual GameState getCurrentGameState() const noexcept = 0;
protected:
    void synchronizeClients();
private:
    std::vector<IClient*> m_subscribers;
};
#endif //LIST_8_IPUBLISHER_HPP
