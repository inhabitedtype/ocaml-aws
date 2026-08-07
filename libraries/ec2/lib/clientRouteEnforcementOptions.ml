open Aws.BaseTypes

type t = { enforced : Boolean.t option }

let make ?enforced () = { enforced }

let parse xml =
  Some { enforced = Aws.Util.option_bind (Aws.Xml.member "Enforced" xml) Boolean.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enforced (fun f ->
             Aws.Query.Pair ("Enforced", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enforced (fun f -> "Enforced", Boolean.to_json f) ])

let of_json j =
  { enforced = Aws.Util.option_map (Aws.Json.lookup j "Enforced") Boolean.of_json }
