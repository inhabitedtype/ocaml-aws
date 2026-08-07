open Aws.BaseTypes

type t = (String.t, Double.t) Hashtbl.t

let make elems () = elems
let parse xml = None
let to_query v = Aws.Query.to_query_hashtbl String.to_string Double.to_query v

let to_json v =
  `Assoc
    (Hashtbl.fold
       (fun k -> fun v -> fun acc -> (String.to_string k, Double.to_json v) :: acc)
       v
       [])

let of_json j = Aws.Json.to_hashtbl String.of_string Double.of_json j
