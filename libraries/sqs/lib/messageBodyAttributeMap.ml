open Aws.BaseTypes

type t = (String.t, MessageAttributeValue.t) Hashtbl.t

let make elems () = elems
let parse xml = None

let to_query v =
  Aws.Query.to_query_hashtbl String.to_string MessageAttributeValue.to_query v

let to_json v =
  `Assoc
    (Hashtbl.fold
       (fun k ->
         fun v -> fun acc -> (String.to_string k, MessageAttributeValue.to_json v) :: acc)
       v
       [])

let of_json j = Aws.Json.to_hashtbl String.of_string MessageAttributeValue.of_json j
