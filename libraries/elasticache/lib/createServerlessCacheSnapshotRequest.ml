open Aws.BaseTypes

type t =
  { serverless_cache_snapshot_name : String.t
  ; serverless_cache_name : String.t
  ; kms_key_id : String.t option
  ; tags : TagList.t
  }

let make
    ~serverless_cache_snapshot_name
    ~serverless_cache_name
    ?kms_key_id
    ?(tags = [])
    () =
  { serverless_cache_snapshot_name; serverless_cache_name; kms_key_id; tags }

let parse xml =
  Some
    { serverless_cache_snapshot_name =
        Aws.Xml.required
          "ServerlessCacheSnapshotName"
          (Aws.Util.option_bind
             (Aws.Xml.member "ServerlessCacheSnapshotName" xml)
             String.parse)
    ; serverless_cache_name =
        Aws.Xml.required
          "ServerlessCacheName"
          (Aws.Util.option_bind (Aws.Xml.member "ServerlessCacheName" xml) String.parse)
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
           (Aws.Query.Pair ("ServerlessCacheName", String.to_query v.serverless_cache_name))
       ; Some
           (Aws.Query.Pair
              ( "ServerlessCacheSnapshotName"
              , String.to_query v.serverless_cache_snapshot_name ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Some ("ServerlessCacheName", String.to_json v.serverless_cache_name)
       ; Some
           ("ServerlessCacheSnapshotName", String.to_json v.serverless_cache_snapshot_name)
       ])

let of_json j =
  { serverless_cache_snapshot_name =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerlessCacheSnapshotName"))
  ; serverless_cache_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "ServerlessCacheName"))
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
