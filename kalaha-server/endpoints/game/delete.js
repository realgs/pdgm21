helperFunctions = require("../../helperFunctions");
constants = require("../../constants");

const deleteGame = (playerId, gameId, games) => {
  if (gameId < games.length) {
    let game = games[gameId];
    if (game.players[playerId]) {
      delete game.players[playerId];
      if (Object.keys(game.players).length === 0) {
        games[gameId] = null;
      } else {
        if (game.is_bot_game) {
          games[gameId] = null;
        } else {
          game.in_progress = false;
          Object.values(game.players).forEach((p) => {
            p.is_first_player = true;
          });
          game.result = null;
          game.moves_next = Object.keys(game.players)[0];
          game.state.first_player_holes = constants.startingArrayOfStones(
            game.number_of_stones
          );
          game.state.second_player_holes = constants.startingArrayOfStones(
            game.number_of_stones
          );
        }
      }
      return { game: null };
    }
  }
  return {};
};

module.exports = {
  deleteGame,
};
