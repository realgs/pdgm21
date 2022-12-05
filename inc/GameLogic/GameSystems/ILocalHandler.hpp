//
// Created by jakubszwedowicz on 05.12.22.
//

#ifndef LIST_8_ILOCALHANDLER_HPP
#define LIST_8_ILOCALHANDLER_HPP

#include "IHandler.hpp"

class ILocalSession;

class ILocalHandler : public IHandler {
public:
    virtual ~ILocalHandler() = default;

private:
};

#endif //LIST_8_ILOCALHANDLER_HPP
