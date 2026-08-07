open Aws.BaseTypes

type t = { enabled : Boolean.t option }

let make ?enabled () = { enabled }

let parse xml =
  Some { enabled = Aws.Util.option_bind (Aws.Xml.member "Enabled" xml) Boolean.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enabled (fun f ->
             Aws.Query.Pair ("Enabled", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.enabled (fun f -> "Enabled", Boolean.to_json f) ])

let of_json j =
  { enabled = Aws.Util.option_map (Aws.Json.lookup j "Enabled") Boolean.of_json }
