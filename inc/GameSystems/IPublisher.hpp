//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_IPUBLISHER_HPP
#define LIST_8_IPUBLISHER_HPP
#include <list>
#include <optional>
#include "ISubscriber.hpp"
#include "Board.hpp"
#include "PlayerScore.hpp"
#include "PlayerSettings.hpp"

class IPublisher
{
public:
    virtual ~IPublisher() = default;
    bool subscribe(ISubscriber& a_subscriber) noexcept;
    virtual std::optional<Board> getBoard(PlayerSettings a_playerSetting) const noexcept = 0;
    virtual std::optional<std::vector<PlayerScore>> getPlayersScore(PlayerSettings a_playerSetting) const noexcept = 0;
protected:
    void synchronizeClients();
private:
    std::list<ISubscriber*> m_subscribers;
};
#endif //LIST_8_IPUBLISHER_HPP
