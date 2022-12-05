import base64
import time
import zmq

server_protocol = "tcp"
server_ip = '127.0.0.1'
server_port = '5556'
server_address = f'{server_protocol}://{server_ip}:{server_port}'
client_msg = 'Message from client'


def zeromsg_example() -> None:
    context = zmq.Context()
    socket = context.socket(zmq.REQ)
    socket.connect(server_address)
    print(f"Connected to the server {server_address}!")

    # send/Request
    start = time.time()
    socket.send_string(client_msg)

    #  Get the reply.
    message = socket.recv_string()
    print(f'Received reply from {server_address} = "{message}"')

    socket.close()

from app import app
def flask_example():
    app.run(debug=True)



if __name__ == '__main__':
    # zeromsg_example()
    flask_example()


# See PyCharm help at https://www.jetbrains.com/help/pycharm/
