open Aws.BaseTypes

type t =
  { serverless_cache_name : String.t option
  ; serverless_cache_snapshot_name : String.t option
  ; snapshot_type : String.t option
  ; next_token : String.t option
  ; max_results : Integer.t option
  }

let make
    ?serverless_cache_name
    ?serverless_cache_snapshot_name
    ?snapshot_type
    ?next_token
    ?max_results
    () =
  { serverless_cache_name
  ; serverless_cache_snapshot_name
  ; snapshot_type
  ; next_token
  ; max_results
  }

let parse xml =
  Some
    { serverless_cache_name =
        Aws.Util.option_bind (Aws.Xml.member "ServerlessCacheName" xml) String.parse
    ; serverless_cache_snapshot_name =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessCacheSnapshotName" xml)
          String.parse
    ; snapshot_type =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotType" xml) String.parse
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    ; max_results = Aws.Util.option_bind (Aws.Xml.member "MaxResults" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f ->
             Aws.Query.Pair ("MaxResults", Integer.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Aws.Util.option_map v.snapshot_type (fun f ->
             Aws.Query.Pair ("SnapshotType", String.to_query f))
       ; Aws.Util.option_map v.serverless_cache_snapshot_name (fun f ->
             Aws.Query.Pair ("ServerlessCacheSnapshotName", String.to_query f))
       ; Aws.Util.option_map v.serverless_cache_name (fun f ->
             Aws.Query.Pair ("ServerlessCacheName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_results (fun f -> "MaxResults", Integer.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Aws.Util.option_map v.snapshot_type (fun f -> "SnapshotType", String.to_json f)
       ; Aws.Util.option_map v.serverless_cache_snapshot_name (fun f ->
             "ServerlessCacheSnapshotName", String.to_json f)
       ; Aws.Util.option_map v.serverless_cache_name (fun f ->
             "ServerlessCacheName", String.to_json f)
       ])

let of_json j =
  { serverless_cache_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessCacheName") String.of_json
  ; serverless_cache_snapshot_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessCacheSnapshotName") String.of_json
  ; snapshot_type = Aws.Util.option_map (Aws.Json.lookup j "SnapshotType") String.of_json
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  ; max_results = Aws.Util.option_map (Aws.Json.lookup j "MaxResults") Integer.of_json
  }
