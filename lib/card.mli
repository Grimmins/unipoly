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

val create_card : string -> string -> action -> card

val get_name : card -> string

val get_description : card -> string

val get_effect : card -> action

val apply_card_effect : player -> card -> player

