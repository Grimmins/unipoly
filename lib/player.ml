type player = {
    name : string;
    position : int; (* En programmation fonctionnel pas de mutable !! *)
    money : int;
    mutable in_jail : bool;
}

(* créer un joueur, initial pos : 0, money : 0, sans propriétés *)
let create_player name = { name; position = 0; money = 1500; in_jail = false;}

(* return pos du joueur *)
let pos_player player = player.position

(* change pos du joueur *)
let change_pos player new_pos = { player with position = new_pos }

let send_to_jail (player: player) =
  player.in_jail <- true

let release_from_jail (player: player) =
  player.in_jail <- false

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