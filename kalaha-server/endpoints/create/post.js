helperFunctions = require("../../helperFunctions");
constants = require("../../constants");

const postCreate = (isBotGame, numberOfStones, playerId, games) => {
  if (
    helperFunctions.checkIfPlayerIsInGame(playerId, games) ||
    isNaN(numberOfStones) ||
    numberOfStones <= 0 ||
    numberOfStones > 82
  ) {
    return {};
  }
  let game = {
    players: {
      [playerId]: {
        player_id: playerId,
        is_first_player: true,
        last_ping: new Date(),
      },
    },
    state: {
      first_player_holes: constants.startingArrayOfStones(numberOfStones),
      second_player_holes: constants.startingArrayOfStones(numberOfStones),
    },
    game_id: games.length,
    result: null,
    number_of_stones: numberOfStones,
    moves_next: playerId,
    in_progress: false,
    is_bot_game: isBotGame,
  };
  if (isBotGame) {
    game.players["bot"] = {
      player_id: "bot",
      is_first_player: false,
    };
  }
  games.push(game);

  return {
    game: {
      ...game,
      state: {
        player_holes: game.state.first_player_holes,
        enemy_holes: game.state.second_player_holes,
      },
    },
  };
};

module.exports = {
  postCreate,
};
