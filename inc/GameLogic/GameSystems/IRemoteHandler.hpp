//
// Created by jakubszwedowicz on 05.12.22.
//

#ifndef LIST_8_IREMOTEHANDLER_HPP
#define LIST_8_IREMOTEHANDLER_HPP
#include "IHandler.hpp"

class IRemoteSession;

class IRemoteHandler : public IHandler {
public:
    virtual ~IRemoteHandler() = default;

private:
};
#endif //LIST_8_IREMOTEHANDLER_HPP
