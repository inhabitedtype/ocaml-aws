open Aws.BaseTypes

type t = (String.t, String.t) Hashtbl.t

let make elems () = elems
let parse xml = None
let to_query v = Aws.Query.to_query_hashtbl String.to_string String.to_query v

let to_json v =
  `Assoc
    (Hashtbl.fold
       (fun k -> fun v -> fun acc -> (String.to_string k, String.to_json v) :: acc)
       v
       [])

let of_json j = Aws.Json.to_hashtbl String.of_string String.of_json j
