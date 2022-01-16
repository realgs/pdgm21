start python -m kalaha server -H 127.0.0.1 -p 20202 -tt 60

@REM start python -m kalaha client -H 127.0.0.1 -p 20202 -ap -apd 10 -md 2
@REM start python -m kalaha client -H 127.0.0.1 -p 20202 -ap -apd 10 -nab -md 6

@REM Player with higher md with ab vs Player with lower md with no ab
start python -m kalaha client -H 127.0.0.1 -p 20202 -ap -apd 5 -nab -md 2
start python -m kalaha client -H 127.0.0.1 -p 20202 -ap -apd 5 -md 4

@REM Alpha-beta pruning disabled, player with higher minimax depth wins or ties
@REM start python -m kalaha client -H 127.0.0.1 -p 20202 -ap -apd 0 -nab -md 2
@REM start python -m kalaha client -H 127.0.0.1 -p 20202 -ap -apd 0 -nab -md 4

@REM Alpha-beta pruning disabled, one player uses iterative deepening
@REM start python -m kalaha client -H 127.0.0.1 -p 20202 -ap -apd 0 -nab -md 4 -id
@REM start python -m kalaha client -H 127.0.0.1 -p 20202 -ap -apd 0 -nab -md 2
