type player

(** [create_player name] creates a player with the name [name].
    @param name the name of the player
    @return player
*)
val create_player : string -> player

(** [pos_player player] returns the position of the player.
    @param player the player
    @return position
*)
val pos_player : player -> int

(** [change_pos player n] changes the position of the player to [n].
    @param player the player
    @param new_pos the new position
    @return player
*)
val change_pos : player -> int -> player

(** [name_player player] returns the name of the player.
    @param player the player
    @return name
*)
val name_player : player -> string

(** [is_in_jail player] returns true if the player is in jail, false otherwise.
    @param player the player
    @return bool
*)
val is_in_jail : player -> bool

(** [money_player player] returns the money of the player.
    @param player the player
    @return money
*)
val money_player : player -> int

(** [change_money player n] changes the money of the player by [n].
    @param player the player
    @param new_money the amount of money to add
    @return player
*)
val change_money : player -> int -> player

(** [receive_alibi_card player] gives an alibi card to the player.
    @param player the player
    @return player
*)
val receive_alibi_card : player -> player

(** [use_alibi_card player] uses an alibi card for the player.
    @param player the player
    @return player
*)
val use_alibi_card : player -> player

(** [can_use_alibi player] returns true if the player can use an alibi card, false otherwise.
    @param player the player
    @return bool
*)
val can_use_alibi : player -> bool

(** [find_index_player player players] returns the index of the player in the array of players.
    @param player the player
    @param players the array of players
    @return index
*)
val find_index_player : player -> player array -> int option

(** [toogle_to_jail player b] changes the jail status of the player to [b].
    @param player the player
    @param b the new jail status
    @return player
*)
val toogle_to_jail : player -> bool -> player

(** [add_turn_jail player] adds a turn in jail for the player.
    @param player the player
    @return player
*)
val add_turn_jail : player -> player

(** [get_turn_jail player] returns the number of turns the player has been in jail.
    @param player the player
    @return number of turns
*)
val get_turn_jail : player -> int

(** [eliminate_player player] eliminates the player from the game.
    @param player the player
    @return player
*)
val eliminate_player : player -> player

(** [is_eliminated player] returns true if the player is eliminated, false otherwise.
    @param player the player
    @return bool
*)
val is_eliminated : player -> bool