type board

(* fonction d'affichage *)
val display :  board -> Player.player array -> unit

(* fonction de création *)
val init_board : unit-> board
