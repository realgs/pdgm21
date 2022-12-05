//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include <string>

#include "IPlayer.hpp"
#include "ISession.hpp"

IPlayer::IPlayer(ISession &a_session, int a_playerId) noexcept: m_session(a_session), m_playerId(a_playerId),
                                                                m_name(a_session.getCurrentGameState().getPlayerName(a_playerId)) {
}

