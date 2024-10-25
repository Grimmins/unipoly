type player

val create_player : string -> player

val pos_player : player -> int

val change_pos : player -> int -> player

val name_player : player -> string

val is_in_jail : player -> bool

val money_player : player -> int

val change_money : player -> int -> player

val send_to_jail : player -> unit

val release_from_jail : player -> unit

val find_index_player : player -> player array -> int option

val toogle_to_jail : player -> bool -> player