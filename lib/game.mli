type game_state = {
  board : Board.board;
  players : Player.player array;
  current_player : Player.player;
}

type play = 
  | Roll
  | Move of int
  | Buy of Square.square_buyable
  
 
type outcome = 
  | Next of game_state
  | Error of Error.error

val act : Player.player -> play -> game_state -> outcome
val create_game : Board.board -> Player.player array -> game_state