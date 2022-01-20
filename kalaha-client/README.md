# [WIP] BASIC KALAHA WEB CLIENT [WIP]

things to do in the future:

- use some real designs so that the page does not hurt my eyes
- cleaner scss -> use constants for colors margins etc
- use pullstate/redux for storing state pulled from api
- add passwords to game creation ?
- add accounts (username, password) then display player nicknames in the lobby, maybe keep track of their record of wins and loses
- better structuring (for example keeping every api call in one api.js file is disgusting)
- ERROR HANDLING!!!
- export validators for input, maybe implement useInput custom hook
- display information about max stones near input
- add posibility to switch places as first/second to move
- add posibility to kick someone from the lobby (to solve: who can kick???)
- get rid of double ternary operators (its ugly)
- add default api handler (onStart onMove etc look quite similar)
- indication of turns switching
- indication of where the move will end (on front and maybe from enemy too)
- some sound maybe indicating your turn?
- make game end if points > half ?
- add bot gameplay with different amount of stones, different difficulties
- tidy the bot code in backend (allow bigger depth)
- add some kind of loading state
- add chat ?
- add responsivity
