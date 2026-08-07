open Aws.BaseTypes

type t = { output : Boolean.t option }

let make ?output () = { output }

let parse xml =
  Some { output = Aws.Util.option_bind (Aws.Xml.member "output" xml) Boolean.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.output (fun f ->
             Aws.Query.Pair ("Output", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.output (fun f -> "output", Boolean.to_json f) ])

let of_json j =
  { output = Aws.Util.option_map (Aws.Json.lookup j "output") Boolean.of_json }
