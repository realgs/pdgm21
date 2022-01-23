//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_SERVER_HPP
#define LIST_8_SERVER_HPP

#include <memory>

#include "IPublisher.hpp"
#include "GameState.hpp"
#include "ISubscriber.hpp"

//class ISubscriber;
class Server : public IPublisher
{
public:
    // CTOR
    Server(const GameState& a_startingGameState) noexcept;

    ~Server() override = default;

    // FUNCTIONS
    void run() noexcept;

    // GETTERS
    GameState getCurrentGameState() const noexcept override;

private:
    void runGame() noexcept;

    void prepareGame() noexcept;

    void annouceTheWinner() const noexcept;

    int oneValidTurn(int a_playerIndex);

    GameState m_startingGameState;
    GameState m_currentGameState;
    std::vector<std::unique_ptr<ISubscriber>> m_clients;
};

#endif //LIST_8_SERVER_HPP
