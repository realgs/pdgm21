//
// Author: jakubszwedowicz
// Date: 21.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST8_GAMESTATE_HPP
#define LIST8_GAMESTATE_HPP
#include <iostream>
#include <vector>

class GameState
{
public:
    GameState(const std::vector<std::string>& a_names, int a_numberOfHumanPlayers = 1, int a_numberOfBotPlayers = 1, int a_numberOfStartingRocks = 6, int a_housesPerSide = 6);
    friend std::ostream& operator<<(std::ostream& a_out, const GameState& a_game) noexcept;
    // FUNCTIONS
    int unloadHouse(int a_house, int a_player) noexcept;
    void finishGame(int a_finishingPlayer) noexcept;
    bool validateMove(int a_move, int a_playerIndex) const noexcept;
    void updateRocksPointsBoardWithOther(GameState&& a_gameState) noexcept;
    void incrementTurnNumber() noexcept;

    // GETTERS
    int getNumberOfPlayers() const noexcept;
    int getNumberOfHumanPayers() const noexcept;
    int getNumberOfBotPlayers() const noexcept;
    int getHolesPerSide() const noexcept;
    int getTurnNumber() const noexcept;
    const std::vector<int>& getPoints() const noexcept;
    const std::vector<int>& getRocksLeft() const noexcept;
    const std::vector<int>& getBoard() const noexcept;
    const std::vector<std::string>& getPlayerNames() const noexcept;
    const std::string& getPlayerName(int a_playerId) const noexcept;

    // SETTERS
    void setPoints(int a_player, int a_newValue) noexcept;
private:
    int m_numberOfHumanPlayers;
    int m_numberOfBotPlayers;
    int m_numberOfPlayers;
    int m_housesPerSide;
    int m_holesPerSide;
    int m_holesOnBoard;
    int m_numberOfStartingRocks;
    int m_currentTurn;
    std::vector<int> m_rocksLeft;
    std::vector<int> m_points;
    std::vector<int> m_board;
    std::vector<std::string> m_playerNames;
};
#endif //LIST8_GAMESTATE_HPP
