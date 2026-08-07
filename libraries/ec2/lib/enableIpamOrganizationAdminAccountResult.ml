open Aws.BaseTypes

type t = { success : Boolean.t option }

let make ?success () = { success }

let parse xml =
  Some { success = Aws.Util.option_bind (Aws.Xml.member "success" xml) Boolean.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.success (fun f ->
             Aws.Query.Pair ("Success", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.success (fun f -> "success", Boolean.to_json f) ])

let of_json j =
  { success = Aws.Util.option_map (Aws.Json.lookup j "success") Boolean.of_json }
