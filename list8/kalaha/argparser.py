import os
import logging
import argparse

from kalaha.config import __version__, __app_name__


def get_parser():
    parser = argparse.ArgumentParser(
        prog=__app_name__,
        formatter_class=argparse.RawDescriptionHelpFormatter,
        description="Kalaha game",
        epilog='See "kalaha <command> -h" for more information '
               'on a specific mode.'
    )

    parser.add_argument('-V', '--version', action='version', version='v{}'.format(__version__),
                        help='print version and exit')
    parser.add_argument('--log-level', default="INFO",
                        choices=['DEBUG', 'INFO', 'WARNING', 'ERROR', 'CRITICAL'],
                        help='set log level')

    subparsers = parser.add_subparsers(
        title='Available modes',
        metavar='',
        dest='mode'
    )

    # building executable
    server_parser = subparsers.add_parser(
        'server',
        aliases=['s'],
        formatter_class=argparse.RawDescriptionHelpFormatter,
        help='Host a game',
        description="Start a server to host a game of Kalaha."
    )

    server_parser.add_argument('-H', '--host', type=str, default='127.0.0.1',
                               help='hostname to bind to')
    server_parser.add_argument('-p', '--port', type=int, default=20202,
                               help='port to listen on')
    server_parser.add_argument('--stones-count', '-sc', type=int, default=5,
                               help='number of stones per pit')
    server_parser.add_argument('--board-size', '-bs', type=int, default=6,
                               help='number of pits per board')
    server_parser.add_argument('--turn-timeout', '-tt', type=int, default=30,
                               help='timeout for each turn')

    client_parser = subparsers.add_parser(
        'client',
        aliases=['c'],
        formatter_class=argparse.RawDescriptionHelpFormatter,
        help='Connect to a server',
        description="Connect to a server to play a game of Kalaha."
    )

    client_parser.add_argument('-H', '--host', type=str, default='127.0.0.1',
                               help='hostname to connect to')
    client_parser.add_argument('-p', '--port', type=int, default=20202,
                               help='port to connect to')
    client_parser.add_argument('--auto-play', '-ap', action='store_true',
                               help='automatically play the game')
    client_parser.add_argument('--minimax-depth', '-md', type=int, default=4,
                               help='minimax depth')
    client_parser.add_argument('--auto-play-delay', '-apd', type=int, default=5,
                               help='delay between moves in auto-play mode in seconds')
    client_parser.add_argument('--no-alpha-beta', '-nab', action='store_true',
                               help='disable alpha-beta pruning')
    client_parser.add_argument('--iterative-deepening', '-id', action='store_true',
                               help='enable iterative deepening')
    return parser


def parse_args(args: list):
    parser = get_parser()
    args = parser.parse_args(args)

    if args.mode is None:
        parser.print_help()
        return
    else:
        args.host = os.getenv("HOST", args.host)
        args.port = int(os.getenv("PORT", args.port))
        args.log_level = os.getenv("LOG_LEVEL", args.log_level)

    if args.mode == "server":
        args.stones_count = int(os.getenv("STONES_COUNT", args.stones_count))
        args.board_size = int(os.getenv("BOARD_SIZE", args.board_size))
        args.turn_timeout = int(os.getenv("TURN_TIMEOUT", args.turn_timeout))
    elif args.mode == "client":
        args.minimax_depth = int(os.getenv("MINIMAX_DEPTH", args.minimax_depth))
        args.auto_play = str(os.getenv("AUTO_PLAY", args.auto_play)).lower() in ('true', '1', 't')
        args.auto_play_delay = int(os.getenv("AUTO_PLAY_DELAY", args.auto_play_delay))
        args.no_alpha_beta = str(os.getenv("NO_ALPHA_BETA", args.no_alpha_beta)).lower() in ('true', '1', 't')
        args.iterative_deepening = str(os.getenv("ITERATIVE_DEEPENING", args.iterative_deepening)).lower() in ('true', '1', 't')
    return args
