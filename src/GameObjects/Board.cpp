//
// Author: jakubszwedowicz
// Date: 19.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#include <iostream>
#include "Board.hpp"
#include "House.hpp"
#include "Mankala.hpp"
#include "../../inc/GameSystems/Client.hpp"

Board::Board(int a_clientsNumber, int a_housesPerSide, int a_rocksPerHouse) noexcept
        : m_housesPerSide(a_housesPerSide)
        , m_rocksPerHouse(a_rocksPerHouse)
        , m_mankalas(a_clientsNumber)
        , m_holesPerSide(a_housesPerSide + 1)
        , m_holesOnBoard(a_clientsNumber * m_holesPerSide)
        , m_housesOnBoard(m_holesOnBoard - m_mankalas)
{
    m_holes.reserve(m_holesOnBoard);
    m_rocks.reserve(m_housesOnBoard * m_rocksPerHouse);
}

Hole* Board::unloadHole(int a_houseIndex) noexcept
{
    std::vector<Rock*> unloadedRocks = m_holes[a_houseIndex]->takeRocks();
    for (auto it: unloadedRocks)
    {
        a_houseIndex++;
        while (!m_holes[a_houseIndex]->addRock(*it));
    }
    Hole* lastHole = m_holes[a_houseIndex].get();
    return lastHole;
}

void Board::init(std::vector<std::unique_ptr<Client>>& a_clients) noexcept
{
    m_holes.clear();
    m_rocks.clear();
    for (auto& client: a_clients)
    {
        for (int _ = 0; _ < m_housesPerSide; _++)
        {
            m_holes.push_back(std::make_unique<House>(House(client.get())));
            Hole& house = *m_holes.back().get();
            for (int i = 0; i < m_rocksPerHouse; i++)
            {
                m_rocks.push_back(Rock(client.get()));
                house.addRock(m_rocks.back());
            }
        }
        m_holes.push_back(std::make_unique<Mankala>(Mankala(client.get())));
    }
}

bool Board::validateMove(int a_move, int a_playerIndex) const noexcept
{
    if (a_move >= a_playerIndex * m_holesPerSide && a_move < (a_playerIndex + 1) * m_holesPerSide - 1)
        if (m_holes[a_move]->getRocksNumber() != 0)
            return true;
    return false;
}

std::ostream& operator<<(std::ostream& a_out, const Board& a_board) noexcept
{   //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |
    //|0 |-----------------------| 0|
    //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |


    //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |
    a_out << "|  |";
    for (int i = a_board.m_holesOnBoard - 2; i >= a_board.m_holesPerSide; i--)
        a_out << " " << a_board.m_holes[i]->getRocksNumber() << " |";
    a_out << "  |" << std::endl;


    //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |
    //|0 |-----------------------| 0|
    a_out << "|" << a_board.m_holes[a_board.m_holesOnBoard - 1]->getRocksNumber() << " |";
    for (int i = 0; i < a_board.m_holesPerSide - 1; i++)
        a_out << "----";
    a_out << "---| " << a_board.m_holes[a_board.m_housesPerSide]->getRocksNumber() << "|" << std::endl;

    //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |
    //|0 |-----------------------| 0|
    //|  | 4 | 4 | 4 | 4 | 4 | 4 |  |
    a_out << "|  |";
    for (int i = 0; i < a_board.m_housesPerSide; i++)
        a_out << " " << a_board.m_holes[i]->getRocksNumber() << " |";
    a_out << "  |" << std::endl;

    return a_out;
}

Board::Board(const Board& a_board) noexcept
        : Board(a_board.m_mankalas, a_board.m_housesPerSide, a_board.m_rocksPerHouse)
{
    for (int player = 0; player < m_mankalas; player++)
    {
        for (int i = player * m_holesPerSide; i < (player + 1) * m_holesPerSide - 1; i++)
        {
            m_holes.push_back(std::make_unique<House>(a_board.m_holes[i]->getOwner()));
            Hole& hole = *m_holes.back().get();
            for (int j = 0; j < a_board.m_holes[i]->getRocksNumber(); j++)
            {
                m_rocks.push_back(Rock(a_board.m_holes[i]->getOwner()));
                hole.addRock(m_rocks.back());
            }
        }
        m_holes.push_back(
                std::make_unique<Mankala>(Mankala(a_board.m_holes[(player + 1) * m_holesPerSide]->getOwner())));
        Hole& mankala = *m_holes.back().get();
        for (int i = 0; i < a_board.m_holes[(player + 1) * m_holesPerSide]->getRocksNumber(); i++)
        {
            m_rocks.push_back(Rock(a_board.m_holes[(player + 1) * m_holesPerSide]->getOwner()));
            mankala.addRock(m_rocks.back());
        }
    }

}


