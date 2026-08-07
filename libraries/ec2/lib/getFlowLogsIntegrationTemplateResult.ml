open Aws.BaseTypes

type t = { result : String.t option }

let make ?result () = { result }

let parse xml =
  Some { result = Aws.Util.option_bind (Aws.Xml.member "result" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.result (fun f ->
             Aws.Query.Pair ("Result", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.result (fun f -> "result", String.to_json f) ])

let of_json j =
  { result = Aws.Util.option_map (Aws.Json.lookup j "result") String.of_json }
