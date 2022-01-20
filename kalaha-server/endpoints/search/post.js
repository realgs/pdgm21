helperFunctions = require("../../helperFunctions");

const postSearch = (playerId, games) => {
  if (!helperFunctions.checkIfPlayerIsInGame(playerId, games)) {
    for (let i = 0; i < games.length; i++) {
      if (
        games[i] &&
        !games[i].is_bot_game &&
        Object.keys(games[i].players).length === 1
      ) {
        let game = games[i];
        game.players[playerId] = {
          player_id: playerId,
          is_first_player: false,
          last_ping: new Date(),
        };
        return {
          game: {
            ...game,
            state: {
              player_holes: game.state.second_player_holes,
              enemy_holes: game.state.first_player_holes,
            },
          },
        };
      }
    }
  }
  return {};
};

module.exports = {
  postSearch,
};
