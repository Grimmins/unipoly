type board = Square.square array

(* fonction d'affichage *)
val display :  board -> Player.player array -> unit

(* fonction de création *)
val init_board : unit-> board

val index_square : Square.square -> Square.square array -> int option
