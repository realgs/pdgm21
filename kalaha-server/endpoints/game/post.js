helperFunctions = require("../../helperFunctions");

const postGame = (playerId, gameId, games) => {
  if (gameId < games.length && games[gameId]) {
    let game = games[gameId];
    if (game.players[playerId]) {
      game.players[playerId].last_ping = new Date();
      return {
        game: {
          ...game,
          state: {
            player_holes: game.players[playerId].is_first_player
              ? game.state.first_player_holes
              : game.state.second_player_holes,
            enemy_holes: game.players[playerId].is_first_player
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
  postGame,
};
