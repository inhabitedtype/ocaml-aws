open Aws.BaseTypes

type t =
  { source_snapshot_name : String.t
  ; target_snapshot_name : String.t
  ; target_bucket : String.t option
  ; kms_key_id : String.t option
  ; tags : TagList.t
  }

let make
    ~source_snapshot_name
    ~target_snapshot_name
    ?target_bucket
    ?kms_key_id
    ?(tags = [])
    () =
  { source_snapshot_name; target_snapshot_name; target_bucket; kms_key_id; tags }

let parse xml =
  Some
    { source_snapshot_name =
        Aws.Xml.required
          "SourceSnapshotName"
          (Aws.Util.option_bind (Aws.Xml.member "SourceSnapshotName" xml) String.parse)
    ; target_snapshot_name =
        Aws.Xml.required
          "TargetSnapshotName"
          (Aws.Util.option_bind (Aws.Xml.member "TargetSnapshotName" xml) String.parse)
    ; target_bucket =
        Aws.Util.option_bind (Aws.Xml.member "TargetBucket" xml) String.parse
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
       ; Aws.Util.option_map v.target_bucket (fun f ->
             Aws.Query.Pair ("TargetBucket", String.to_query f))
       ; Some
           (Aws.Query.Pair ("TargetSnapshotName", String.to_query v.target_snapshot_name))
       ; Some
           (Aws.Query.Pair ("SourceSnapshotName", String.to_query v.source_snapshot_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.target_bucket (fun f -> "TargetBucket", String.to_json f)
       ; Some ("TargetSnapshotName", String.to_json v.target_snapshot_name)
       ; Some ("SourceSnapshotName", String.to_json v.source_snapshot_name)
       ])

let of_json j =
  { source_snapshot_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceSnapshotName"))
  ; target_snapshot_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetSnapshotName"))
  ; target_bucket = Aws.Util.option_map (Aws.Json.lookup j "TargetBucket") String.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
