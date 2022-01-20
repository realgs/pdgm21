import ky from "ky";

const apiClient = ky.create({
  prefixUrl: "https://kalaha-server.herokuapp.com/",
});

export const apiPostGame = (playerId, gameId) =>
  apiClient
    .post("game", { json: { player_id: playerId, game_id: gameId } })
    .json();

export const apiDeleteGame = (playerId, gameId) =>
  apiClient
    .delete("game", { json: { player_id: playerId, game_id: gameId } })
    .json();

export const apiPostSession = () => apiClient.post("session").json();

export const apiPostStart = (playerId, gameId) =>
  apiClient
    .post("start", { json: { player_id: playerId, game_id: gameId } })
    .json();

export const apiPostCreateBot = (numberOfStones, playerId) =>
  apiClient
    .post("create", {
      json: {
        number_of_stones: parseInt(numberOfStones),
        player_id: playerId,
        is_bot_game: true,
      },
    })
    .json();

export const apiPostSearch = (playerId) =>
  apiClient.post("search", { json: { player_id: playerId } }).json();

export const apiPostCreate = (numberOfStones, playerId) =>
  apiClient
    .post("create", {
      json: {
        number_of_stones: parseInt(numberOfStones),
        player_id: playerId,
        is_bot_game: false,
      },
    })
    .json();

export const apiPostJoin = (playerId, gameId) =>
  apiClient
    .post("join", { json: { player_id: playerId, game_id: gameId } })
    .json();

export const apiPostMove = (move, playerId, gameId) =>
  apiClient
    .post("move", {
      json: { move: move, player_id: playerId, game_id: gameId },
    })
    .json();
