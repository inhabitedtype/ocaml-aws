open Aws.BaseTypes

type t = { target_resource_count : Integer.t option }

let make ?target_resource_count () = { target_resource_count }

let parse xml =
  Some
    { target_resource_count =
        Aws.Util.option_bind (Aws.Xml.member "targetResourceCount" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_resource_count (fun f ->
             Aws.Query.Pair ("TargetResourceCount", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_resource_count (fun f ->
             "targetResourceCount", Integer.to_json f)
       ])

let of_json j =
  { target_resource_count =
      Aws.Util.option_map (Aws.Json.lookup j "targetResourceCount") Integer.of_json
  }
