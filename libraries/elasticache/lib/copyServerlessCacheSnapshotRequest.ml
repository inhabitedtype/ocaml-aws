open Aws.BaseTypes

type t =
  { source_serverless_cache_snapshot_name : String.t
  ; target_serverless_cache_snapshot_name : String.t
  ; kms_key_id : String.t option
  ; tags : TagList.t
  }

let make
    ~source_serverless_cache_snapshot_name
    ~target_serverless_cache_snapshot_name
    ?kms_key_id
    ?(tags = [])
    () =
  { source_serverless_cache_snapshot_name
  ; target_serverless_cache_snapshot_name
  ; kms_key_id
  ; tags
  }

let parse xml =
  Some
    { source_serverless_cache_snapshot_name =
        Aws.Xml.required
          "SourceServerlessCacheSnapshotName"
          (Aws.Util.option_bind
             (Aws.Xml.member "SourceServerlessCacheSnapshotName" xml)
             String.parse)
    ; target_serverless_cache_snapshot_name =
        Aws.Xml.required
          "TargetServerlessCacheSnapshotName"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetServerlessCacheSnapshotName" xml)
             String.parse)
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TargetServerlessCacheSnapshotName"
              , String.to_query v.target_serverless_cache_snapshot_name ))
       ; Some
           (Aws.Query.Pair
              ( "SourceServerlessCacheSnapshotName"
              , String.to_query v.source_serverless_cache_snapshot_name ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Some
           ( "TargetServerlessCacheSnapshotName"
           , String.to_json v.target_serverless_cache_snapshot_name )
       ; Some
           ( "SourceServerlessCacheSnapshotName"
           , String.to_json v.source_serverless_cache_snapshot_name )
       ])

let of_json j =
  { source_serverless_cache_snapshot_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceServerlessCacheSnapshotName"))
  ; target_serverless_cache_snapshot_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetServerlessCacheSnapshotName"))
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
