open Aws.BaseTypes

type t =
  { cache_cluster_id : String.t
  ; final_snapshot_identifier : String.t option
  }

let make ~cache_cluster_id ?final_snapshot_identifier () =
  { cache_cluster_id; final_snapshot_identifier }

let parse xml =
  Some
    { cache_cluster_id =
        Aws.Xml.required
          "CacheClusterId"
          (Aws.Util.option_bind (Aws.Xml.member "CacheClusterId" xml) String.parse)
    ; final_snapshot_identifier =
        Aws.Util.option_bind (Aws.Xml.member "FinalSnapshotIdentifier" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.final_snapshot_identifier (fun f ->
             Aws.Query.Pair ("FinalSnapshotIdentifier", String.to_query f))
       ; Some (Aws.Query.Pair ("CacheClusterId", String.to_query v.cache_cluster_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.final_snapshot_identifier (fun f ->
             "FinalSnapshotIdentifier", String.to_json f)
       ; Some ("CacheClusterId", String.to_json v.cache_cluster_id)
       ])

let of_json j =
  { cache_cluster_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "CacheClusterId"))
  ; final_snapshot_identifier =
      Aws.Util.option_map (Aws.Json.lookup j "FinalSnapshotIdentifier") String.of_json
  }
