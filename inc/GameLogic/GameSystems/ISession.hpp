//
// Created by jakubszwedowicz on 05.12.22.
//

#ifndef LIST_8_ISESSION_HPP
#define LIST_8_ISESSION_HPP

#include <memory>

#include "GameState.hpp"
#include "GameSessionSate.hpp"
#include "IPlayer.hpp"
#include "PlayerMove.hpp"

class IHandler;
class ISession
{
public:
    virtual ~ISession() = default;
    ISession(IHandler& a_handler, int a_sessionId, const GameState& a_gameState);
    const GameState& getCurrentGameState() const noexcept;

    GameSessionSate runGame() noexcept;



    GameSessionSate getCurrentGameSessionState() const noexcept;

    void annouceTheWinner() const noexcept;

    int oneValidTurn(int a_playerIndex);

    bool applyMove(PlayerMove&& a_playerMove) noexcept;

private:
    void prepareGame() noexcept;


    IHandler& m_handler;
    GameSessionSate m_currentGameSessionState;
    int m_sessionId;
    PlayerMove m_currentPlayerMove;
    GameState m_startingGameState;
    GameState m_currentGameState;
    std::vector<std::unique_ptr<IPlayer>> m_players;
};
#endif //LIST_8_ISESSION_HPP
