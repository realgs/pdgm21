import React, { useState, useEffect } from "react";

import { GameScreen } from "features/GameScreen";
import { MainScreen } from "features/MainScreen";
import { apiPostSession } from "utils/api.js";

export const MainRouter = () => {
  const [game, setGame] = useState(null);
  const [playerId, setPlayerId] = useState("");

  useEffect(() => {
    if (localStorage.getItem("game") && localStorage.getItem("playerId")) {
      const game = JSON.parse(localStorage.getItem("game"));
      const playerId = JSON.parse(localStorage.getItem("playerId"));
      setGame(game);
      setPlayerId(playerId);
    } else if (!playerId) {
      apiPostSession()
        .then((res) => {
          if (res.player_id) {
            setPlayerId(res.player_id);
            localStorage.setItem("playerId", JSON.stringify(res.player_id));
          }
        })
        .catch((err) => {
          console.log(err); // TODO: HANDLE ERRORS
        });
    }
  }, [playerId]);

  return game ? (
    <GameScreen game={game} playerId={playerId} setGame={setGame} />
  ) : playerId ? (
    <MainScreen setGame={setGame} playerId={playerId} />
  ) : (
    <></> // TODO SERVER NETWORK ERROR SCREEN
  );
};
