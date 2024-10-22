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

val receive_alibi_card : player -> unit

val use_alibi_card : player -> unit

val can_use_alibi : player -> bool

val find_index_player : player -> player array -> int option