open Aws.BaseTypes

type t = { value : Double.t option }

let make ?value () = { value }

let parse xml =
  Some { value = Aws.Util.option_bind (Aws.Xml.member "Value" xml) Double.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f ->
             Aws.Query.Pair ("Value", Double.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f -> "Value", Double.to_json f) ])

let of_json j = { value = Aws.Util.option_map (Aws.Json.lookup j "Value") Double.of_json }
