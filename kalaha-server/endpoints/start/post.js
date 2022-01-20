helperFunctions = require("../../helperFunctions");
constants = require("../../constants");

const postStart = (playerId, gameId, games) => {
  if (gameId < games.length && games[gameId]) {
    let game = games[gameId];
    if (game.players[playerId] && Object.keys(game.players).length === 2) {
      game.in_progress = true;
      game.moves_next = Object.keys(game.players)[0];
      game.result = null;
      game.state.first_player_holes = constants.startingArrayOfStones(
        game.number_of_stones
      );
      game.state.second_player_holes = constants.startingArrayOfStones(
        game.number_of_stones
      );
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
  postStart,
};
