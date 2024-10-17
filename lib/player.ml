type player = {
    name : string;
    position : int; (* En programmation fonctionnel pas de mutable !! *)
    (*money : int;*)
}

(* créer un joueur, initial pos : 0, money : 0, sans propriétés *)
let create_player name = { name; position = 0; (*money = 0;*)}

(* return pos du joueur *)
let pos_player player = player.position

(* change pos du joueur *)
let change_pos player new_pos = { player with position = new_pos }

(* return nom joueur *)
let name_player player = player.name