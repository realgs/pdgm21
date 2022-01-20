//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//
#include "PlayerScore.hpp"


PlayerScore::PlayerScore(int a_score) noexcept : m_score(a_score)
{

}

bool PlayerScore::setScore(int a_newScore) noexcept
{
    if(a_newScore < 0) return false;
    m_score = a_newScore;
    return true;
}


void PlayerScore::increaseScore(int a_increase) noexcept
{
    m_score += a_increase;
}

int PlayerScore::getScore() const noexcept
{
    return m_score;
}


