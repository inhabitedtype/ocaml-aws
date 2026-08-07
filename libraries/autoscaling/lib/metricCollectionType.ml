open Aws.BaseTypes

type t = { metric : String.t option }

let make ?metric () = { metric }

let parse xml =
  Some { metric = Aws.Util.option_bind (Aws.Xml.member "Metric" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric (fun f ->
             Aws.Query.Pair ("Metric", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.metric (fun f -> "Metric", String.to_json f) ])

let of_json j =
  { metric = Aws.Util.option_map (Aws.Json.lookup j "Metric") String.of_json }
