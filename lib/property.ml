open Square

let count_properties_owned property_type player_index board =
  Array.fold_left (fun count square ->
    match square with
    | Buyable { type_square = ts; proprietaire_index = Some index } when index = player_index && property_type ts -> count + 1
    | _ -> count
  ) 0 board

let is_restaurant = function
  | Restaurant _ -> true
  | _ -> false

let is_library = function
  | Library _ -> true
  | _ -> false

let is_cours = function
  | Cours _ -> true
  | _ -> false

let count_restaurants_owned player_index board =
    count_properties_owned is_restaurant player_index board

let count_librairies_owned player_index board =
    count_properties_owned is_library player_index board

let count_cours_owned player_index board =
    count_properties_owned is_cours player_index board