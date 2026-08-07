open Aws.BaseTypes

type t = { value : Blob.t option }

let make ?value () = { value }

let parse xml =
  Some { value = Aws.Util.option_bind (Aws.Xml.member "value" xml) Blob.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f -> Aws.Query.Pair ("Value", Blob.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.value (fun f -> "value", Blob.to_json f) ])

let of_json j = { value = Aws.Util.option_map (Aws.Json.lookup j "value") Blob.of_json }
