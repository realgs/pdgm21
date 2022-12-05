//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_IPLAYER_HPP
#define LIST_8_IPLAYER_HPP

class ISession;
class IPlayer
{
public:
    IPlayer(ISession& a_session, int a_playerId) noexcept;
    virtual ~IPlayer() = default;
    virtual int getMove() noexcept = 0;
    virtual bool acceptMove(int a_move) noexcept = 0;
    virtual bool declineMove(int a_move) noexcept = 0;
protected:
    ISession& m_session;
    // Add enum with player state or maybe Session should have states like "human player's move", "waiting for response" etc.
    int m_playerId;
    std::string m_name;
};
#endif //LIST_8_IPLAYER_HPP
