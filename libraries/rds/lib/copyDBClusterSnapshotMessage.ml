open Aws.BaseTypes

type t =
  { source_d_b_cluster_snapshot_identifier : String.t
  ; target_d_b_cluster_snapshot_identifier : String.t
  ; kms_key_id : String.t option
  ; pre_signed_url : String.t option
  ; copy_tags : Boolean.t option
  ; tags : TagList.t
  }

let make
    ~source_d_b_cluster_snapshot_identifier
    ~target_d_b_cluster_snapshot_identifier
    ?kms_key_id
    ?pre_signed_url
    ?copy_tags
    ?(tags = [])
    () =
  { source_d_b_cluster_snapshot_identifier
  ; target_d_b_cluster_snapshot_identifier
  ; kms_key_id
  ; pre_signed_url
  ; copy_tags
  ; tags
  }

let parse xml =
  Some
    { source_d_b_cluster_snapshot_identifier =
        Aws.Xml.required
          "SourceDBClusterSnapshotIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "SourceDBClusterSnapshotIdentifier" xml)
             String.parse)
    ; target_d_b_cluster_snapshot_identifier =
        Aws.Xml.required
          "TargetDBClusterSnapshotIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetDBClusterSnapshotIdentifier" xml)
             String.parse)
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; pre_signed_url =
        Aws.Util.option_bind (Aws.Xml.member "PreSignedUrl" xml) String.parse
    ; copy_tags = Aws.Util.option_bind (Aws.Xml.member "CopyTags" xml) Boolean.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.copy_tags (fun f ->
             Aws.Query.Pair ("CopyTags", Boolean.to_query f))
       ; Aws.Util.option_map v.pre_signed_url (fun f ->
             Aws.Query.Pair ("PreSignedUrl", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TargetDBClusterSnapshotIdentifier"
              , String.to_query v.target_d_b_cluster_snapshot_identifier ))
       ; Some
           (Aws.Query.Pair
              ( "SourceDBClusterSnapshotIdentifier"
              , String.to_query v.source_d_b_cluster_snapshot_identifier ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.copy_tags (fun f -> "CopyTags", Boolean.to_json f)
       ; Aws.Util.option_map v.pre_signed_url (fun f -> "PreSignedUrl", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Some
           ( "TargetDBClusterSnapshotIdentifier"
           , String.to_json v.target_d_b_cluster_snapshot_identifier )
       ; Some
           ( "SourceDBClusterSnapshotIdentifier"
           , String.to_json v.source_d_b_cluster_snapshot_identifier )
       ])

let of_json j =
  { source_d_b_cluster_snapshot_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceDBClusterSnapshotIdentifier"))
  ; target_d_b_cluster_snapshot_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetDBClusterSnapshotIdentifier"))
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; pre_signed_url = Aws.Util.option_map (Aws.Json.lookup j "PreSignedUrl") String.of_json
  ; copy_tags = Aws.Util.option_map (Aws.Json.lookup j "CopyTags") Boolean.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  }
