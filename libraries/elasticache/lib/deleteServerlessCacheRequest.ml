open Aws.BaseTypes

type t =
  { serverless_cache_name : String.t
  ; final_snapshot_name : String.t option
  }

let make ~serverless_cache_name ?final_snapshot_name () =
  { serverless_cache_name; final_snapshot_name }

let parse xml =
  Some
    { serverless_cache_name =
        Aws.Xml.required
          "ServerlessCacheName"
          (Aws.Util.option_bind (Aws.Xml.member "ServerlessCacheName" xml) String.parse)
    ; final_snapshot_name =
        Aws.Util.option_bind (Aws.Xml.member "FinalSnapshotName" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.final_snapshot_name (fun f ->
             Aws.Query.Pair ("FinalSnapshotName", String.to_query f))
       ; Some
           (Aws.Query.Pair ("ServerlessCacheName", String.to_query v.serverless_cache_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.final_snapshot_name (fun f ->
             "FinalSnapshotName", String.to_json f)
       ; Some ("ServerlessCacheName", String.to_json v.serverless_cache_name)
       ])

let of_json j =
  { serverless_cache_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerlessCacheName"))
  ; final_snapshot_name =
      Aws.Util.option_map (Aws.Json.lookup j "FinalSnapshotName") String.of_json
  }
