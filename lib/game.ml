
type outcome = 
  | Next of game_state
  | Error of error
  | Endgame of player option

