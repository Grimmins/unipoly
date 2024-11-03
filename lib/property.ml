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

let owns_all_courses_in_ufr ufr player_index board =
  Array.for_all (fun square ->
    match square with
    | Buyable { type_square = Cours cours; proprietaire_index = Some index }
      when (get_ufr cours) = ufr && index = player_index -> true
    | Buyable { type_square = Cours cours; _ } when (get_ufr cours) = ufr -> false
    | _ -> true
  ) board

let get_courses_owned_by_player player_index board =
  Array.fold_left (fun acc square ->
    match square with
    | Buyable { type_square = Cours _cours; proprietaire_index = Some index; _ } when index = player_index ->
        square :: acc
    | _ -> acc
  ) [] board

let get_landing_price cours =
  match (get_degre cours) with
  | None -> get_landing_price cours
  | Some Licence -> get_licence_price cours
  | Some Master -> get_master_price cours
  | Some Doctorat -> get_doctorat_price cours

let get_adjusted_course_landing_price cours player_index board =
  let base_price = get_landing_price cours in
  if (get_degre cours) = None && owns_all_courses_in_ufr (get_ufr cours) player_index board then
    base_price * 2
  else
    base_price

let get_next_degree_price cours =
    match (get_degre cours) with
        | Some Doctorat -> 0
        | _ -> get_upgrade_price cours