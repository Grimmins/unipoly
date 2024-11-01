type board = Square.square array

(* fonction d'affichage *)
val display :  board -> Player.player array -> int -> unit

(* fonction de création *)
val init_board : unit-> board

val index_square : Square.square -> Square.square array -> int option

val get_square : int -> board -> Square.square

val change_square : int -> Square.square -> board -> unit