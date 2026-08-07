open Aws.BaseTypes

type t = { principal : String.t option }

let make ?principal () = { principal }

let parse xml =
  Some { principal = Aws.Util.option_bind (Aws.Xml.member "Principal" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.principal (fun f ->
             Aws.Query.Pair ("Principal", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.principal (fun f -> "Principal", String.to_json f) ])

let of_json j =
  { principal = Aws.Util.option_map (Aws.Json.lookup j "Principal") String.of_json }
