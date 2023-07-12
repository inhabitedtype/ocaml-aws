type t =
  | List of t list
  | Pair of (string * t)
  | Value of string option

let render q =
  let rec enc k q =
    match k, q with
    | k, List xs -> List.concat (List.map (enc k) xs)
    | Some n, Pair (label, subq) -> enc (Some (n ^ "." ^ label)) subq
    | None, Pair (label, subq) -> enc (Some label) subq
    | Some n, Value (Some s) -> [ n ^ "=" ^ Uri.pct_encode ~component:`Query_value s ]
    | None, Value (Some s) -> [ Uri.pct_encode s ]
    | Some s, _ -> [ s ]
    | _ -> []
  in
  String.concat "&" (enc None q)

let to_query_list to_query vals =
  let i = ref 0 in
  List
    (List.map
       (fun v ->
         i := !i + 1;
         Pair (string_of_int !i, to_query v))
       vals)

let to_query_hashtbl key_to_str to_query tbl =
  List (Hashtbl.fold (fun k v acc -> Pair (key_to_str k, to_query v) :: acc) tbl [])
