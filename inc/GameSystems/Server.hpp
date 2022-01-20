//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_SERVER_HPP
#define LIST_8_SERVER_HPP

#include "../GameUtils/Settings.hpp"
#include "IPublisher.hpp"
#include "Client.hpp"

class Server : public IPublisher
{
public:
    // CTOR
    Server() noexcept;

    // FUNCTIONS
    void run(Settings* a_settings) noexcept;

    // GETTERS
    std::optional<Board> getBoard(PlayerSettings a_playerSetting) const noexcept override;

    std::optional<std::vector<PlayerScore>> getPlayersScore(PlayerSettings a_playerSetting) const noexcept override;

private:
    void runGame() noexcept;

    bool validateMove(int a_move, int a_playerIndex) const noexcept;

    void prepareGame(Settings* a_settings) noexcept;

    void changeSettings(Settings* a_settings) noexcept;

    void applySettings() noexcept;

    void init() noexcept;

    bool verifyPlayerSettings(const PlayerSettings& a_playerSettings) const noexcept;

    bool m_settingsChanged;
    Settings* m_settings;
    std::vector<PlayerScore> m_playersScores;
    std::vector<std::unique_ptr<Client>> m_clients;
    std::unique_ptr<Board> m_board;
};

#endif //LIST_8_SERVER_HPP
