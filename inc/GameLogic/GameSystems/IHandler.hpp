//
// Created by jakubszwedowicz on 05.12.22.
//

#ifndef LIST_8_IHANDLER_HPP
#define LIST_8_IHANDLER_HPP
#include <vector>

#include "ILocalSession.hpp"
#include "IRemoteSession.hpp"

class IHandler {
public:
    virtual ~IHandler() = default;
    virtual void createLocalGame() = 0;
    virtual void createRemoteGame() = 0;
protected:
};

#endif //LIST_8_IHANDLER_HPP
