open Aws.BaseTypes

type t =
  { serverless_cache_snapshot_name : String.t option
  ; a_r_n : String.t option
  ; kms_key_id : String.t option
  ; snapshot_type : String.t option
  ; status : String.t option
  ; create_time : DateTime.t option
  ; expiry_time : DateTime.t option
  ; bytes_used_for_cache : String.t option
  ; serverless_cache_configuration : ServerlessCacheConfiguration.t option
  }

let make
    ?serverless_cache_snapshot_name
    ?a_r_n
    ?kms_key_id
    ?snapshot_type
    ?status
    ?create_time
    ?expiry_time
    ?bytes_used_for_cache
    ?serverless_cache_configuration
    () =
  { serverless_cache_snapshot_name
  ; a_r_n
  ; kms_key_id
  ; snapshot_type
  ; status
  ; create_time
  ; expiry_time
  ; bytes_used_for_cache
  ; serverless_cache_configuration
  }

let parse xml =
  Some
    { serverless_cache_snapshot_name =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessCacheSnapshotName" xml)
          String.parse
    ; a_r_n = Aws.Util.option_bind (Aws.Xml.member "ARN" xml) String.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; snapshot_type =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotType" xml) String.parse
    ; status = Aws.Util.option_bind (Aws.Xml.member "Status" xml) String.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "CreateTime" xml) DateTime.parse
    ; expiry_time = Aws.Util.option_bind (Aws.Xml.member "ExpiryTime" xml) DateTime.parse
    ; bytes_used_for_cache =
        Aws.Util.option_bind (Aws.Xml.member "BytesUsedForCache" xml) String.parse
    ; serverless_cache_configuration =
        Aws.Util.option_bind
          (Aws.Xml.member "ServerlessCacheConfiguration" xml)
          ServerlessCacheConfiguration.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.serverless_cache_configuration (fun f ->
             Aws.Query.Pair
               ("ServerlessCacheConfiguration", ServerlessCacheConfiguration.to_query f))
       ; Aws.Util.option_map v.bytes_used_for_cache (fun f ->
             Aws.Query.Pair ("BytesUsedForCache", String.to_query f))
       ; Aws.Util.option_map v.expiry_time (fun f ->
             Aws.Query.Pair ("ExpiryTime", DateTime.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", String.to_query f))
       ; Aws.Util.option_map v.snapshot_type (fun f ->
             Aws.Query.Pair ("SnapshotType", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.a_r_n (fun f -> Aws.Query.Pair ("ARN", String.to_query f))
       ; Aws.Util.option_map v.serverless_cache_snapshot_name (fun f ->
             Aws.Query.Pair ("ServerlessCacheSnapshotName", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.serverless_cache_configuration (fun f ->
             "ServerlessCacheConfiguration", ServerlessCacheConfiguration.to_json f)
       ; Aws.Util.option_map v.bytes_used_for_cache (fun f ->
             "BytesUsedForCache", String.to_json f)
       ; Aws.Util.option_map v.expiry_time (fun f -> "ExpiryTime", DateTime.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "CreateTime", DateTime.to_json f)
       ; Aws.Util.option_map v.status (fun f -> "Status", String.to_json f)
       ; Aws.Util.option_map v.snapshot_type (fun f -> "SnapshotType", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.a_r_n (fun f -> "ARN", String.to_json f)
       ; Aws.Util.option_map v.serverless_cache_snapshot_name (fun f ->
             "ServerlessCacheSnapshotName", String.to_json f)
       ])

let of_json j =
  { serverless_cache_snapshot_name =
      Aws.Util.option_map (Aws.Json.lookup j "ServerlessCacheSnapshotName") String.of_json
  ; a_r_n = Aws.Util.option_map (Aws.Json.lookup j "ARN") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; snapshot_type = Aws.Util.option_map (Aws.Json.lookup j "SnapshotType") String.of_json
  ; status = Aws.Util.option_map (Aws.Json.lookup j "Status") String.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "CreateTime") DateTime.of_json
  ; expiry_time = Aws.Util.option_map (Aws.Json.lookup j "ExpiryTime") DateTime.of_json
  ; bytes_used_for_cache =
      Aws.Util.option_map (Aws.Json.lookup j "BytesUsedForCache") String.of_json
  ; serverless_cache_configuration =
      Aws.Util.option_map
        (Aws.Json.lookup j "ServerlessCacheConfiguration")
        ServerlessCacheConfiguration.of_json
  }
