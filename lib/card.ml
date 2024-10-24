open Player

type action =
  | Move of int (* move player of n squares *)
  | GainMoney of int  (* player win some money *)
  | LoseMoney of int  (* player loose some money *)
  | GoTo of int  (* player sent to a specific square *)
  | GoToJail
 (* | SkipTurn   player pass turn *)
  | GetOutOfJail  (* alibi card for cheat *)

(* def card *)
type card = {
  name: string;
  description: string;  (* description card *)
  effect: action;  (* action effect *)
}

(* Fonctions pour créer une carte *)
let create_card name description effect = { name; description; effect }

(* Fonctions pour accéder aux champs privés *)
let get_name card = card.name
let get_description card = card.description
let get_effect card = card.effect

let apply_card_effect (player: player) (card: card) : player =
    match card.effect with
      | Move n -> change_pos player (pos_player player + n)
      | GainMoney amount -> change_money player (money_player player + amount)
      | LoseMoney amount -> change_money player (money_player player - amount)
      | GoTo position -> change_pos player position
      | GoToJail -> send_to_jail player; change_pos player 10
     (* | SkipTurn -> { player with skip_turn = true }  on add possibilité de skip turn ? *)
    (*  | GetOutOfJail -> receive_alibi_card player  Le joueur reçoit une carte spéciale *)