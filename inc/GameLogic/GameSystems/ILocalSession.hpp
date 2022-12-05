//
// Created by jakubszwedowicz on 05.12.22.
//

#ifndef LIST_8_ILOCALSESSION_HPP
#define LIST_8_ILOCALSESSION_HPP
#include "ISession.hpp"

class ILocalHandler;
class ILocalSession : ISession
{
public:
    ILocalSession(ILocalHandler& a_handler, int a_sessionId);
    virtual ~ILocalSession() = default;
    bool subscribe(ILocalHandler& a_remoteHandler) noexcept;
    virtual GameState getCurrentGameState() const noexcept = 0;
protected:
    void synchronizeClients();
private:
    ILocalHandler& remoteHandler;
};

#endif //LIST_8_ILOCALSESSION_HPP
