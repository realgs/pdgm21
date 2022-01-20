//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_BOUARD_HPP
#define LIST_8_BOUARD_HPP
#include <vector>
#include "Hole.hpp"

class Client;
class Board
{
public:
    // CTORs
    Board() noexcept = default;
    Board(int a_clientsNumber, int a_housesPerSide, int a_rocksPerHouse) noexcept;
    Board(const Board& a_board) noexcept;
    // FUNCTIONS
    void init(std::vector<std::unique_ptr<Client>>& a_client) noexcept;
    Hole* unloadHole(int a_houseIndex) noexcept;
    bool validateMove(int a_move, int a_playerIndex) const noexcept;
    friend std::ostream& operator<<(std::ostream& a_out, const Board& a_board) noexcept;
    // GETTERS

    // SETTERS
private:
    int m_housesPerSide;
    int m_rocksPerHouse;
    int m_mankalas;
    int m_holesPerSide;
    int m_holesOnBoard;
    int m_housesOnBoard;
    std::vector<std::unique_ptr<Hole>> m_holes;
    std::vector<Rock> m_rocks;

};
#endif //LIST_8_BOUARD_HPP
