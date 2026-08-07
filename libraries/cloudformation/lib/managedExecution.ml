open Aws.BaseTypes

type t = { active : Boolean.t option }

let make ?active () = { active }

let parse xml =
  Some { active = Aws.Util.option_bind (Aws.Xml.member "Active" xml) Boolean.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.active (fun f ->
             Aws.Query.Pair ("Active", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.active (fun f -> "Active", Boolean.to_json f) ])

let of_json j =
  { active = Aws.Util.option_map (Aws.Json.lookup j "Active") Boolean.of_json }
