open Aws.BaseTypes

type t = { id : String.t option }

let make ?id () = { id }
let parse xml = Some { id = Aws.Util.option_bind (Aws.Xml.member "Id" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.id (fun f -> Aws.Query.Pair ("Id", String.to_query f)) ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.id (fun f -> "Id", String.to_json f) ])

let of_json j = { id = Aws.Util.option_map (Aws.Json.lookup j "Id") String.of_json }
