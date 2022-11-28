#include <zmq.hpp>
#include <thread>

#include "GamesHandler.hpp"


void zeromsgExample()
{
    using namespace std::chrono_literals;

    // initialize the zmq context with a single IO thread
    zmq::context_t context{1};

    // construct a REP (reply) socket and bind to interface
    zmq::socket_t socket{context, zmq::socket_type::rep};
    socket.bind("tcp://127.0.0.1:5556");

    // prepare some static data for responses
    const std::string data{"Response from server"};

    for (;;)
    {
        zmq::message_t request;

        // receive a request from client
        (void)socket.recv(request, zmq::recv_flags::none);
        std::cout << "Received '" << request.to_string() << "'" << std::endl;

        // simulate work
        std::this_thread::sleep_for(1s);

        // send the reply to the client
        socket.send(zmq::buffer(data), zmq::send_flags::none);
    }

}

int main()
{
//    GamesHandler sys;
//    sys.startSystem();
    zeromsgExample();
}
