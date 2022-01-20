import React, { useState } from "react";

import styles from "./styles.module.scss";
import { MenuButton } from "shared/components/MenuButton";
import {
  apiPostCreate,
  apiPostSearch,
  apiPostJoin,
  apiPostCreateBot,
} from "utils/api";

export const MainScreen = ({ playerId, setGame }) => {
  const [gameToJoinId, setGameToJoinId] = useState("");
  const [numberOfStones, setNumberOfStones] = useState(6);

  const onCreate = () => {
    if (!isNaN(numberOfStones) && numberOfStones > 0 && numberOfStones <= 82) {
      apiPostCreate(numberOfStones, playerId)
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
    } else {
      // TODO: SHOW VALIDATION ERROR
    }
  };

  const onSearch = () => {
    apiPostSearch(playerId)
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

  const onJoin = () => {
    if (!isNaN(parseInt(gameToJoinId))) {
      apiPostJoin(playerId, gameToJoinId)
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

  const onStartBot = () => {
    apiPostCreateBot(6, playerId)
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

  return (
    <div className={styles.cMainScreen}>
      <div className={styles.menu}>
        <div className={styles.title}>kalaha</div>
        <div className={styles.inputContainer}></div>
        <div className={styles.buttonContainer}>
          <MenuButton onClick={onStartBot} caption="Play with bot" />
          <div className={styles.inputContainer}>
            <MenuButton onClick={onJoin} caption="Join specific game" />
            <div className={styles.inputInfoText}>Game id:</div>
            <input
              className={styles.joinInput}
              value={gameToJoinId}
              onChange={(e) => {
                setGameToJoinId(e.target.value);
              }}
            />
          </div>
          <div className={styles.middleContainer}>
            <MenuButton onClick={onSearch} caption="Join random game" />
          </div>
          <div className={styles.inputContainer}>
            <MenuButton onClick={onCreate} caption="Create game" />
            <div className={styles.inputInfoText}>Number of stones:</div>
            <input
              type="number"
              className={styles.joinInput}
              value={numberOfStones}
              onChange={(e) => {
                setNumberOfStones(e.target.value);
              }}
            />
          </div>
        </div>
      </div>
    </div>
  );
};
