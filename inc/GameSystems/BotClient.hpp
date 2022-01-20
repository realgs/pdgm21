//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_BOTCLIENT_HPP
#define LIST_8_BOTCLIENT_HPP
#include "Client.hpp"

class BotClient : public Client
{
public:
    BotClient(const PlayerSettings& a_playerSettings, std::vector<PlayerScore>& a_playersScores, Board& a_board, IPublisher& a_publisher) noexcept;

//    const Board& getBoard() const noexcept override;
//
//    const PlayerScore& getPlayerScore() const noexcept override;

    int makeTurn() noexcept override;

    void updateBoard() noexcept override;

    void updatePlayer() noexcept override;

private:
    std::vector<PlayerScore>& m_playersScores;
    const PlayerSettings& m_playerSettings;
    Board& m_board;
};
#endif //LIST_8_BOTCLIENT_HPP