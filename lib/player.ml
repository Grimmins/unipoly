type player = {
    name : string;
    position : int; (* En programmation fonctionnel pas de mutable !! *)
    money : int;
    in_jail : bool;
    turn_in_jail : int;
    has_alibi :  bool;
}

(* créer un joueur, initial pos : 0, money : 0, sans propriétés *)
let create_player name = { name; position = 0; money = 1500; in_jail = false; turn_in_jail = 0; has_alibi = false}

(* return pos du joueur *)
let pos_player player = player.position

(* change pos du joueur *)
let change_pos player new_pos =
    let adjusted_pos = (new_pos + 40) mod 40 in
    { player with position = adjusted_pos }

let receive_alibi_card (player: player) = { player with has_alibi = true}

let use_alibi_card (player: player) = { player with has_alibi = false}

let can_use_alibi (player: player) = player.has_alibi

(* return nom joueur *)
let name_player player = player.name

(* return is_jail joueur *)
let is_in_jail player = player.in_jail

(* return argent joueur *)
let money_player player = player.money

(* change argent joueur *)
let change_money player new_money = { player with money = player.money + new_money }

let find_index_player player players : int option =
    let n = Array.length players in
    let rec loop i =
      if i = n then None
      else if name_player players.(i) == name_player player then Some i
      else loop (i + 1) in
    loop 0

let toogle_to_jail player b = {player with in_jail = b}

let add_turn_jail player = {player with turn_in_jail = player.turn_in_jail + 1}

let get_turn_jail player = player.turn_in_jail