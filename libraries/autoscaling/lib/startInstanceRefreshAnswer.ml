open Aws.BaseTypes

type t = { instance_refresh_id : String.t option }

let make ?instance_refresh_id () = { instance_refresh_id }

let parse xml =
  Some
    { instance_refresh_id =
        Aws.Util.option_bind (Aws.Xml.member "InstanceRefreshId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_refresh_id (fun f ->
             Aws.Query.Pair ("InstanceRefreshId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_refresh_id (fun f ->
             "InstanceRefreshId", String.to_json f)
       ])

let of_json j =
  { instance_refresh_id =
      Aws.Util.option_map (Aws.Json.lookup j "InstanceRefreshId") String.of_json
  }
