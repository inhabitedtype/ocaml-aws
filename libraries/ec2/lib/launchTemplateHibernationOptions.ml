open Aws.BaseTypes

type t = { configured : Boolean.t option }

let make ?configured () = { configured }

let parse xml =
  Some
    { configured = Aws.Util.option_bind (Aws.Xml.member "configured" xml) Boolean.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.configured (fun f ->
             Aws.Query.Pair ("Configured", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.configured (fun f -> "configured", Boolean.to_json f) ])

let of_json j =
  { configured = Aws.Util.option_map (Aws.Json.lookup j "configured") Boolean.of_json }
