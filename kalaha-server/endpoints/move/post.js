helperFunctions = require("../../helperFunctions");

const postMove = (move, playerId, gameId, games) => {
  if (gameId < games.length && games[gameId] && games[gameId].in_progress) {
    game = games[gameId];
    if (game.players[playerId] && game.moves_next === playerId) {
      const isFirstPlayer = game.players[playerId].is_first_player;

      const playerState = isFirstPlayer
        ? game.state.first_player_holes
        : game.state.second_player_holes;
      const enemyState = isFirstPlayer
        ? game.state.second_player_holes
        : game.state.first_player_holes;

      if (playerState[move] !== 0) {
        helperFunctions.processMove(
          move,
          playerId,
          game,
          playerState,
          enemyState
        );

        if (game.moves_next === "bot" && !game.result) {
          const stateOfBot = playerId === "bot" ? playerState : enemyState;
          const stateOfPlayer = playerId === "bot" ? enemyState : playerState;

          setTimeout(
            () =>
              postMove(
                helperFunctions.decideBotMove(
                  games[gameId],
                  stateOfPlayer,
                  stateOfBot,
                  playerId
                ),
                "bot",
                gameId,
                games
              ),
            3000
          );
        }
      }

      return {
        game: {
          ...game,
          state: {
            player_holes: isFirstPlayer
              ? game.state.first_player_holes
              : game.state.second_player_holes,
            enemy_holes: isFirstPlayer
              ? game.state.second_player_holes
              : game.state.first_player_holes,
          },
        },
      };
    }
  }
  return {};
};

module.exports = {
  postMove,
};
