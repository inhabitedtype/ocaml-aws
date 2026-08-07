open Aws.BaseTypes

type t = { log_group : String.t option }

let make ?log_group () = { log_group }

let parse xml =
  Some { log_group = Aws.Util.option_bind (Aws.Xml.member "LogGroup" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.log_group (fun f ->
             Aws.Query.Pair ("LogGroup", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.log_group (fun f -> "LogGroup", String.to_json f) ])

let of_json j =
  { log_group = Aws.Util.option_map (Aws.Json.lookup j "LogGroup") String.of_json }
