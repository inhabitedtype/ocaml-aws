open Aws.BaseTypes

type t = { object_key : String.t option }

let make ?object_key () = { object_key }

let parse xml =
  Some { object_key = Aws.Util.option_bind (Aws.Xml.member "objectKey" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.object_key (fun f ->
             Aws.Query.Pair ("ObjectKey", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.object_key (fun f -> "objectKey", String.to_json f) ])

let of_json j =
  { object_key = Aws.Util.option_map (Aws.Json.lookup j "objectKey") String.of_json }
