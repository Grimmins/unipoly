type player

val create_player : string -> player

val pos_player : player -> int

val change_pos : player -> int -> player

val name_player : player -> string

val is_in_jail : player -> bool

val money_player : player -> int

val change_money : player -> int -> player

val receive_alibi_card : player -> player

val use_alibi_card : player -> player

val can_use_alibi : player -> bool

val find_index_player : player -> player array -> int option

val toogle_to_jail : player -> bool -> player

val add_turn_jail : player -> player

val get_turn_jail : player -> int