open Aws.BaseTypes

type t = { version : Long.t option }

let make ?version () = { version }

let parse xml =
  Some { version = Aws.Util.option_bind (Aws.Xml.member "version" xml) Long.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.version (fun f ->
             Aws.Query.Pair ("Version", Long.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.version (fun f -> "version", Long.to_json f) ])

let of_json j =
  { version = Aws.Util.option_map (Aws.Json.lookup j "version") Long.of_json }
