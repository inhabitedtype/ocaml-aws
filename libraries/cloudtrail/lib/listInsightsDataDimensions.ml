open Aws.BaseTypes

type t = (ListInsightsDataDimensionKey.t, String.t) Hashtbl.t

let make elems () = elems
let parse xml = None

let to_query v =
  Aws.Query.to_query_hashtbl ListInsightsDataDimensionKey.to_string String.to_query v

let to_json v =
  `Assoc
    (Hashtbl.fold
       (fun k ->
         fun v ->
          fun acc -> (ListInsightsDataDimensionKey.to_string k, String.to_json v) :: acc)
       v
       [])

let of_json j =
  Aws.Json.to_hashtbl ListInsightsDataDimensionKey.of_string String.of_json j
