open Aws.BaseTypes

type t = { source_ipam_pool_id : String.t option }

let make ?source_ipam_pool_id () = { source_ipam_pool_id }

let parse xml =
  Some
    { source_ipam_pool_id =
        Aws.Util.option_bind (Aws.Xml.member "SourceIpamPoolId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_ipam_pool_id (fun f ->
             Aws.Query.Pair ("SourceIpamPoolId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.source_ipam_pool_id (fun f ->
             "SourceIpamPoolId", String.to_json f)
       ])

let of_json j =
  { source_ipam_pool_id =
      Aws.Util.option_map (Aws.Json.lookup j "SourceIpamPoolId") String.of_json
  }
