//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include "HumanClient.hpp"

#include <utility>
#include "PlayerSettings.hpp"
#include "PlayerScore.hpp"
#include "Server.hpp"
#include "HumanPlayer.hpp"

HumanClient::HumanClient(PlayerSettings a_playerSettings, std::vector<PlayerScore> a_playersScores, IPublisher& a_publisher) noexcept
        : m_playerSettings(std::move(a_playerSettings)), m_playersScores(std::move(a_playersScores)), Client(a_publisher), m_board()
{
    m_player = std::make_unique<HumanPlayer>(m_playerSettings.getName(), m_board, m_playersScores);
}

//const Board& HumanClient::getBoard() const noexcept
//{
//    return m_board;
//}
//
//const PlayerScore& HumanClient::getPlayerScore() const noexcept
//{
//    return m_playerScore;
//}

void HumanClient::updateBoard() noexcept
{
    std::optional<Board> maybeBoard = m_publisher.getBoard(m_playerSettings);
    if(maybeBoard)
        m_board = std::move(maybeBoard.value());
}

void HumanClient::updatePlayer() noexcept
{
    std::optional<std::vector<PlayerScore>> maybePlayersScores = m_publisher.getPlayersScore(m_playerSettings);
    if(maybePlayersScores)
        m_playersScores = std::move(maybePlayersScores.value());
}

int HumanClient::makeTurn() noexcept
{
    if (m_playerSettings.getPlayerID() == 1)
        std::cout << "Turn of player: " << m_playerSettings.getName() << std::endl;
    std::cout << m_board << std::endl;
    if (m_playerSettings.getPlayerID() == 0)
        std::cout << "Turn of player: " << m_playerSettings.getName() << std::endl;
    return m_player->makeTurn();
}
