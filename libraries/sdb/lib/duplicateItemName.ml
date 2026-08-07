open Aws.BaseTypes

type t = { box_usage : Float.t option }

let make ?box_usage () = { box_usage }

let parse xml =
  Some { box_usage = Aws.Util.option_bind (Aws.Xml.member "BoxUsage" xml) Float.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.box_usage (fun f ->
             Aws.Query.Pair ("BoxUsage", Float.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.box_usage (fun f -> "BoxUsage", Float.to_json f) ])

let of_json j =
  { box_usage = Aws.Util.option_map (Aws.Json.lookup j "BoxUsage") Float.of_json }
