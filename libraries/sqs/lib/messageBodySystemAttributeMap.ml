type t = (MessageSystemAttributeNameForSends.t, MessageSystemAttributeValue.t) Hashtbl.t

let make elems () = elems
let parse xml = None

let to_query v =
  Aws.Query.to_query_hashtbl
    MessageSystemAttributeNameForSends.to_string
    MessageSystemAttributeValue.to_query
    v

let to_json v =
  `Assoc
    (Hashtbl.fold
       (fun k ->
         fun v ->
          fun acc ->
           ( MessageSystemAttributeNameForSends.to_string k
           , MessageSystemAttributeValue.to_json v )
           :: acc)
       v
       [])

let of_json j =
  Aws.Json.to_hashtbl
    MessageSystemAttributeNameForSends.of_string
    MessageSystemAttributeValue.of_json
    j
