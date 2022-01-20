//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include "BotClient.hpp"
#include "PlayerSettings.hpp"
#include "IPublisher.hpp"
#include "BotPlayer.hpp"

BotClient::BotClient(const PlayerSettings& a_playerSettings, std::vector<PlayerScore>& a_playersScores, Board& a_board, IPublisher& a_publisher) noexcept
        : m_playerSettings(a_playerSettings), m_playersScores(a_playersScores), Client(a_publisher), m_board(a_board)
{
    m_player = std::make_unique<BotPlayer>(m_playerSettings.getName(), m_board, a_playersScores);
}

//const Board& BotClient::getBoard() const noexcept
//{
//    return m_board;
//}
//
//const PlayerScore& BotClient::getPlayerScore() const noexcept
//{
//    return m_playerScore;
//}

void BotClient::updateBoard() noexcept
{
    // Does nothing since BotClient runs on Server and is directly connected to Server resources via references
}

void BotClient::updatePlayer() noexcept
{
    // Does nothing since BotClient runs on Server and is directly connected to Server resources via references
}

int BotClient::makeTurn() noexcept
{
    return 0;
}
