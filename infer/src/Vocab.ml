let (>>>) f g = fun x -> f (g x)

let rec reverse : 'a list -> 'a list
=fun lst -> 
  match lst with
  | [] -> []
  | hd::tl -> (reverse tl) @ [hd]

let rec take : int -> 'a list -> 'a list 
=fun k lst ->
  match k, lst with
  | 0, _ -> []
  | _, [] -> []
  | _, hd::tl -> hd::(take (k-1) tl)

let link_by_sep sep s acc = if String.equal acc "" then s else acc ^ sep ^ s

let string_of_set fold set to_string = 
  let add_string v acc = link_by_sep "," (to_string v) acc in
  "{" ^ fold add_string set "" ^ "}"

let string_of_list lst to_string = 
  let add_string acc v = link_by_sep "," (to_string v) acc in
  "[" ^ List.fold ~f:add_string lst ~init:"" ^ "]"
