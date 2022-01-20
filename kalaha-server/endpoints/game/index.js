post = require("./post");
deleteGame = require("./delete");

module.exports = {
  post: post.postGame,
  delete: deleteGame.deleteGame,
};
