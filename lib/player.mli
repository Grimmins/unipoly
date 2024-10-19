type player

val create_player : string -> player

val pos_player : player -> int

val change_pos : player -> int -> player

val name_player : player -> string

val money_player : player -> int

val change_money : player -> int -> player

val find_index_player : player -> player array -> int option