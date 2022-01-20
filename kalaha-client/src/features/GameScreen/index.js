import React, { Component } from "react";
import cn from "classnames";

import {
  apiDeleteGame,
  apiPostGame,
  apiPostStart,
  apiPostMove,
} from "utils/api";
import { MenuButton } from "shared/components/MenuButton";
import { Hole } from "shared/components/Hole";
import styles from "./styles.module.scss";

export class GameScreen extends Component {
  state = {
    interval: null,
  };

  onLeave = () => {
    const { game, playerId, setGame } = this.props;
    clearInterval(this.state.interval);
    apiDeleteGame(playerId, game.game_id)
      .then((res) => {
        if (res.game) {
          setGame(res.game);
        } else {
          setGame(null);
        }
        localStorage.removeItem("game");
      })
      .catch((err) => {
        console.log(err); // TODO: HANDLE ERRORS
      });
  };

  onStart = () => {
    const { game, playerId, setGame } = this.props;
    if (Object.keys(game.players).length === 2) {
      apiPostStart(playerId, game.game_id)
        .then((res) => {
          if (res.game) {
            setGame(res.game);
            localStorage.setItem("game", JSON.stringify(res.game));
          } else {
            setGame(null);
          }
        })
        .catch((err) => {
          console.log(err); // TODO: HANDLE ERRORS
        });
    }
  };

  onMove = (move) => {
    const { game, playerId, setGame } = this.props;
    if (
      game.in_progress &&
      game.state.player_holes[move] &&
      game.moves_next === playerId
    ) {
      apiPostMove(move, playerId, game.game_id)
        .then((res) => {
          if (res.game) {
            setGame(res.game);
            localStorage.setItem("game", JSON.stringify(res.game));
          } else {
            setGame(null);
          }
        })
        .catch((err) => {
          console.log(err); // TODO: HANDLE ERRORS
        });
    }
  };

  getGame = () => {
    const { game, playerId, setGame } = this.props;
    apiPostGame(playerId, game.game_id)
      .then((res) => {
        if (res.game) {
          setGame(res.game);
          localStorage.setItem("game", JSON.stringify(res.game));
        } else {
          setGame(null);
        }
      })
      .catch((err) => {
        console.log(err); // TODO: HANDLE ERRORS
      });
  };

  componentDidMount() {
    this.getGame();
    this.setState({ interval: setInterval(this.getGame, 1000) });
  }

  componentWillUnmount() {
    clearInterval(this.state.interval);
  }

  render() {
    const { game, playerId } = this.props;
    const enemyHoles = game.state.enemy_holes;
    const playerHoles = game.state.player_holes;
    return (
      <div className={styles.cGameScreen}>
        <div className={styles.sidebar}>
          {!game.is_bot_game && (
            <div className={styles.playerCounter}>{`Players: ${
              Object.keys(game.players).length
            }/2`}</div>
          )}
          {!game.in_progress && (
            <MenuButton
              caption={game.result ? "Restart game" : "Start game"}
              onClick={this.onStart}
            />
          )}
          <MenuButton caption="Leave game" onClick={this.onLeave} />
        </div>
        <div className={styles.mainContainer}>
          <div className={styles.infoText}>Game id: {game.game_id}</div>
          {game.result && (
            <div
              className={cn(styles.infoText, {
                [styles.losingText]: game.result !== "draw",
                [styles.winningText]: game.result === playerId,
              })}
            >
              {game.result === "draw"
                ? "It's a draw!"
                : game.result === playerId
                ? "You won!"
                : "You lost!"}
            </div>
          )}
          <div
            className={cn(styles.gameBoard, {
              [styles.gameBoardOnMove]:
                game.moves_next === playerId && !game.result,
            })}
          >
            <Hole type="home" count={enemyHoles[enemyHoles.length - 1]} />
            {playerHoles.map((value, i) => {
              if (i < playerHoles.length - 1) {
                return (
                  <div className={styles.column} key={i}>
                    <Hole
                      type="normal"
                      count={enemyHoles[enemyHoles.length - 2 - i]}
                    />
                    <Hole
                      type="normal"
                      className={styles.playerHole}
                      count={playerHoles[i]}
                      onClick={() => this.onMove(i)}
                    />
                  </div>
                );
              }
              return <div key={i}></div>;
            })}
            <Hole type="home" count={playerHoles[playerHoles.length - 1]} />
          </div>
        </div>
      </div>
    );
  }
}
