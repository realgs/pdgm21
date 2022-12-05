//
// Created by jakubszwedowicz on 05.12.22.
//

#ifndef LIST_8_PLAYERMOVE_HPP
#define LIST_8_PLAYERMOVE_HPP

#include "PlayerMoveState.hpp"

struct PlayerMove {
    PlayerMove(int a_sessionId, int a_playerId, int a_move, PlayerMoveState a_moveState) : m_sessionId(a_sessionId),
                                                                                           m_playerId(a_playerId),
                                                                                           m_move(a_move),
                                                                                           m_moveState(a_moveState) {}

    int m_sessionId;
    int m_playerId;
    int m_move;
    PlayerMoveState m_moveState;
};

#endif //LIST_8_PLAYERMOVE_HPP
