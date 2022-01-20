gameEndpoints = require("./endpoints/game");

const checkIfPlayerIsInGame = (playerId, games) => {
  for (let i = 0; i < games.length; i++) {
    if (games[i]) {
      for (let j = 0; j < games[i].players.length; j++) {
        if (games[i].players[j][player_id]) {
          return true;
        }
      }
    }
  }
  return false;
};

const checkIfPlayerHasNoStones = (arrayOfStones) => {
  for (let i = 0; i < arrayOfStones.length - 1; i++) {
    if (arrayOfStones[i] !== 0) {
      return false;
    }
  }
  return true;
};

const kickUnresponsivePlayers = (games) => {
  for (let i = 0; i < games.length; i++) {
    if (games[i]) {
      Object.values(games[i].players).forEach((p) => {
        if (p.last_ping && new Date() - p.last_ping >= 20000) {
          gameEndpoints.delete(p.player_id, games[i].game_id, games);
        }
      });
    }
  }
};

const processMove = (move, playerId, game, playerState, enemyState) => {
  let index = move + 1;
  let amountOfMoves = playerState[move];
  playerState[move] = 0;

  game.moves_next = game.players[playerId].is_first_player
    ? Object.keys(game.players)[1]
    : Object.keys(game.players)[0];

  while (amountOfMoves !== 0 && game.in_progress) {
    while (index < playerState.length && amountOfMoves !== 0) {
      amountOfMoves--;
      if (
        amountOfMoves === 0 &&
        playerState[index] === 0 &&
        index !== playerState.length - 1 &&
        enemyState[enemyState.length - index - 2] !== 0
      ) {
        playerState[playerState.length - 1] +=
          enemyState[enemyState.length - index - 2] + 1;
        enemyState[enemyState.length - index - 2] = 0;
      } else {
        playerState[index] += 1;
      }
      index++;
    }
    if (amountOfMoves === 0) {
      if (checkIfPlayerHasNoStones(enemyState)) {
        for (let i = 0; i < playerState.length - 1; i++) {
          playerState[playerState.length - 1] += playerState[i];
          playerState[i] = 0;
        }
        if (
          playerState[playerState.length - 1] ===
          enemyState[enemyState.length - 1]
        ) {
          game.result = "draw";
        } else {
          game.result =
            playerState[playerState.length - 1] >
            enemyState[enemyState.length - 1]
              ? playerId
              : game.moves_next;
        }
        game.in_progress = false;
      } else if (index === playerState.length) {
        if (!checkIfPlayerHasNoStones(playerState)) {
          game.moves_next = playerId;
        }
      }
    }
    index = 0;
    while (index < enemyState.length && amountOfMoves !== 0) {
      enemyState[index] += 1;
      index++;
      amountOfMoves--;
    }
    index = 0;
  }
};

const calculateBotMove = (move, game, playerState, botState) => {
  const imaginaryPlayerState = [];
  const imaginaryBotState = [];

  for (let i = 0; i < botState.length; i++) {
    imaginaryBotState[i] = botState[i];
    imaginaryPlayerState[i] = playerState[i];
  }

  while (game.moves_next === "bot") {
    processMove(move, "bot", game, imaginaryBotState, imaginaryPlayerState);
  }

  return {
    player: imaginaryPlayerState,
    bot: imaginaryBotState,
  };
};

const calculatePlayerMove = (move, game, playerState, botState, playerId) => {
  const imaginaryPlayerState = [];
  const imaginaryBotState = [];

  for (let i = 0; i < botState.length; i++) {
    imaginaryBotState[i] = botState[i];
    imaginaryPlayerState[i] = playerState[i];
  }

  while (game.moves_next !== "bot") {
    processMove(move, playerId, game, imaginaryPlayerState, imaginaryBotState);
  }

  return {
    player: imaginaryPlayerState,
    bot: imaginaryBotState,
  };
};

const decideBotMove = (game, playerState, botState, playerId) => {
  if (game) {
    let bestWorstScenario = {
      move: 0,
      value: -Infinity,
    };

    for (let i = 0; i < botState.length - 1; i++) {
      if (botState[i] !== 0) {
        bestWorstScenario.move = i;
      }
    }

    for (let i = 0; i < botState.length - 1; i++) {
      game.moves_next = "bot";
      let oldState = calculateBotMove(i, game, playerState, botState);

      let worstScenarioScore = Infinity;
      for (let j = 0; j < oldState.player.length - 1; j++) {
        game.moves_next = playerId;
        let newState = calculatePlayerMove(
          j,
          game,
          oldState.player,
          oldState.bot,
          playerId
        );
        if (
          newState.bot[newState.bot.length - 1] -
            newState.player[newState.player.length - 1] <
          worstScenarioScore
        ) {
          worstScenarioScore =
            newState.bot[newState.bot.length - 1] -
            newState.player[newState.player.length - 1];
        }
      }
      if (worstScenarioScore > bestWorstScenario.value && botState[i] !== 0) {
        bestWorstScenario.move = i;
        bestWorstScenario.value = worstScenarioScore;
      }
    }
    game.moves_next = "bot";
    game.in_progress = true;
    game.result = null;
    return bestWorstScenario.move;
  }
};

module.exports = {
  checkIfPlayerIsInGame,
  checkIfPlayerHasNoStones,
  kickUnresponsivePlayers,
  decideBotMove,
  processMove,
};
