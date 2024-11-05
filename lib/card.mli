open Player

type action =
  | Move of int (* move player of n squares *)
  | GainMoney of int  (* player win some money *)
  | LoseMoney of int  (* player loose some money *)
  | GoTo of int  (* player sent to a specific square *)
  | GoToJail
 (* | SkipTurn   player pass turn *)
  | GetOutOfJail  (* alibi card for cheat *)

type card

(** [create_card] creates a card with the name [name], the description [description] and the effect [effect].
    @param name the name of the card
    @param description the description of the card
    @param effect the effect of the card
    @return card
*)
val create_card : string -> string -> action -> card

(** [get_name] returns the name of the card.
    @param card the card
    @return name
*)
val get_name : card -> string

(** [get_description] returns the description of the card.
    @param card the card
    @return description
*)
val get_description : card -> string

(** [get_effect] returns the effect of the card.
    @param card the card
    @return effect
*)
val get_effect : card -> action

(** [apply_card_effect player card] applies the effect of the card to the player.
    @param player the player
    @param card the card
    @return player * bool
*)
val apply_card_effect : player -> card -> player * bool

