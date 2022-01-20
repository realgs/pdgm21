//
// Author: jakubszwedowicz
// Date: 20.01.2022.
// e-mail: kuba.szwedowicz@gmail.com 
//

#ifndef LIST_8_PLAYERSCORE_HPP
#define LIST_8_PLAYERSCORE_HPP
class PlayerScore
{
public:
    PlayerScore(int a_score) noexcept;
    bool setScore(int a_newScore) noexcept;
    void increaseScore(int a_increase) noexcept;
    int getScore() const noexcept;
private:
    int m_score;
};
#endif //LIST_8_PLAYERSCORE_HPP
