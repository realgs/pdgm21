// TODO: MAYBE REFACTOR TO DENO?

const express = require("express");
const cors = require("cors");
const uuid = require("uuid");
const bodyParser = require("body-parser");

const PORT = process.env.PORT || 3001;

const app = express();
app.use(cors());
app.use(bodyParser.json());

app.listen(PORT, () => {
  console.log(`Server is listening on port ${PORT}`);
});

helperFunctions = require("./helperFunctions");
const { game, search, create, start, join, move } = require("./endpoints");

let games = [];

setInterval(() => helperFunctions.kickUnresponsivePlayers(games), 10000);

app.post("/session", (_, res) => {
  res.json({ player_id: uuid.v4() });
});

app.post("/game", (req, res) => {
  res.json(game.post(req.body.player_id, parseInt(req.body.game_id), games)); // TODO: HANDLE ERRORS
});

app.delete("/game", (req, res) => {
  res.json(game.delete(req.body.player_id, parseInt(req.body.game_id), games)); // TODO: HANDLE ERRORS
});

app.post("/search", (req, res) => {
  res.json(search.post(req.body.player_id, games)); // TODO: HANDLE ERRORS
});

app.post("/create", (req, res) => {
  res.json(
    create.post(
      req.body.is_bot_game,
      req.body.number_of_stones,
      req.body.player_id,
      games
    )
  ); // TODO: HANDLE ERRORS
});

app.post("/start", (req, res) => {
  res.json(start.post(req.body.player_id, parseInt(req.body.game_id), games)); // TODO: HANDLE ERRORS
});

app.post("/join", (req, res) => {
  res.json(join.post(req.body.player_id, parseInt(req.body.game_id), games)); // TODO: HANDLE ERRORS
});

app.post("/move", (req, res) => {
  res.json(
    move.post(
      req.body.move,
      req.body.player_id,
      parseInt(req.body.game_id),
      games
    )
  ); // TODO: HANDLE ERRORS
});
