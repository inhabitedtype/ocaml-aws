open Aws.BaseTypes

type t = { instance_id : String.t option }

let make ?instance_id () = { instance_id }

let parse xml =
  Some
    { instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f) ])

let of_json j =
  { instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json }
