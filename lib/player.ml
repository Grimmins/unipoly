type player = {
    name : string;
    mutable position : int;
    money : int;
    properties : int list;
}

(* créer un joueur, initial pos : 0, money : 0, sans propriétés *)
let create_player name = { name; position = 0; money = 0; properties = [] }

(* return pos du joueur *)
let pos_player player = player.position

(* change pos du joueur *)
let change_pos player new_pos =
    player.position <- new_pos;
    player

(* return nom joueur *)
let name_player player = player.name