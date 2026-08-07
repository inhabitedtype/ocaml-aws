open Aws.BaseTypes

type t = { name : String.t option }

let make ?name () = { name }

let parse xml =
  Some { name = Aws.Util.option_bind (Aws.Xml.member "Name" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> Aws.Query.Pair ("Name", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.name (fun f -> "Name", String.to_json f) ])

let of_json j = { name = Aws.Util.option_map (Aws.Json.lookup j "Name") String.of_json }
