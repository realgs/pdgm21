//
// Created by jakubszwedowicz on 05.12.22.
//

#ifndef LIST_8_IREMOTESESSION_HPP
#define LIST_8_IREMOTESESSION_HPP
#include "ISession.hpp"

class IRemoteHandler;
class IRemoteSession : ISession
{
public:
    IRemoteSession(IRemoteHandler& a_handler, int a_sessionId);
    virtual ~IRemoteSession() = default;
    bool subscribe(IRemoteHandler& a_remoteHandler) noexcept;
    virtual GameState getCurrentGameState() const noexcept = 0;
protected:
    void synchronizeClients();
private:
    IRemoteHandler& remoteHandler;
};
#endif //LIST_8_IREMOTESESSION_HPP
