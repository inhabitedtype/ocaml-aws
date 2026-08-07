open Aws.BaseTypes

type t =
  { source_d_b_snapshot_identifier : String.t
  ; target_d_b_snapshot_identifier : String.t
  ; kms_key_id : String.t option
  ; tags : TagList.t
  ; copy_tags : Boolean.t option
  ; pre_signed_url : String.t option
  ; option_group_name : String.t option
  ; target_custom_availability_zone : String.t option
  ; snapshot_target : String.t option
  ; copy_option_group : Boolean.t option
  ; snapshot_availability_zone : String.t option
  }

let make
    ~source_d_b_snapshot_identifier
    ~target_d_b_snapshot_identifier
    ?kms_key_id
    ?(tags = [])
    ?copy_tags
    ?pre_signed_url
    ?option_group_name
    ?target_custom_availability_zone
    ?snapshot_target
    ?copy_option_group
    ?snapshot_availability_zone
    () =
  { source_d_b_snapshot_identifier
  ; target_d_b_snapshot_identifier
  ; kms_key_id
  ; tags
  ; copy_tags
  ; pre_signed_url
  ; option_group_name
  ; target_custom_availability_zone
  ; snapshot_target
  ; copy_option_group
  ; snapshot_availability_zone
  }

let parse xml =
  Some
    { source_d_b_snapshot_identifier =
        Aws.Xml.required
          "SourceDBSnapshotIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "SourceDBSnapshotIdentifier" xml)
             String.parse)
    ; target_d_b_snapshot_identifier =
        Aws.Xml.required
          "TargetDBSnapshotIdentifier"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetDBSnapshotIdentifier" xml)
             String.parse)
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "Tags" xml) TagList.parse)
    ; copy_tags = Aws.Util.option_bind (Aws.Xml.member "CopyTags" xml) Boolean.parse
    ; pre_signed_url =
        Aws.Util.option_bind (Aws.Xml.member "PreSignedUrl" xml) String.parse
    ; option_group_name =
        Aws.Util.option_bind (Aws.Xml.member "OptionGroupName" xml) String.parse
    ; target_custom_availability_zone =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetCustomAvailabilityZone" xml)
          String.parse
    ; snapshot_target =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotTarget" xml) String.parse
    ; copy_option_group =
        Aws.Util.option_bind (Aws.Xml.member "CopyOptionGroup" xml) Boolean.parse
    ; snapshot_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "SnapshotAvailabilityZone" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot_availability_zone (fun f ->
             Aws.Query.Pair ("SnapshotAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.copy_option_group (fun f ->
             Aws.Query.Pair ("CopyOptionGroup", Boolean.to_query f))
       ; Aws.Util.option_map v.snapshot_target (fun f ->
             Aws.Query.Pair ("SnapshotTarget", String.to_query f))
       ; Aws.Util.option_map v.target_custom_availability_zone (fun f ->
             Aws.Query.Pair ("TargetCustomAvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.option_group_name (fun f ->
             Aws.Query.Pair ("OptionGroupName", String.to_query f))
       ; Aws.Util.option_map v.pre_signed_url (fun f ->
             Aws.Query.Pair ("PreSignedUrl", String.to_query f))
       ; Aws.Util.option_map v.copy_tags (fun f ->
             Aws.Query.Pair ("CopyTags", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("Tags.member", TagList.to_query v.tags))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "TargetDBSnapshotIdentifier"
              , String.to_query v.target_d_b_snapshot_identifier ))
       ; Some
           (Aws.Query.Pair
              ( "SourceDBSnapshotIdentifier"
              , String.to_query v.source_d_b_snapshot_identifier ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.snapshot_availability_zone (fun f ->
             "SnapshotAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.copy_option_group (fun f ->
             "CopyOptionGroup", Boolean.to_json f)
       ; Aws.Util.option_map v.snapshot_target (fun f ->
             "SnapshotTarget", String.to_json f)
       ; Aws.Util.option_map v.target_custom_availability_zone (fun f ->
             "TargetCustomAvailabilityZone", String.to_json f)
       ; Aws.Util.option_map v.option_group_name (fun f ->
             "OptionGroupName", String.to_json f)
       ; Aws.Util.option_map v.pre_signed_url (fun f -> "PreSignedUrl", String.to_json f)
       ; Aws.Util.option_map v.copy_tags (fun f -> "CopyTags", Boolean.to_json f)
       ; Some ("Tags", TagList.to_json v.tags)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Some
           ("TargetDBSnapshotIdentifier", String.to_json v.target_d_b_snapshot_identifier)
       ; Some
           ("SourceDBSnapshotIdentifier", String.to_json v.source_d_b_snapshot_identifier)
       ])

let of_json j =
  { source_d_b_snapshot_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "SourceDBSnapshotIdentifier"))
  ; target_d_b_snapshot_identifier =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetDBSnapshotIdentifier"))
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Tags"))
  ; copy_tags = Aws.Util.option_map (Aws.Json.lookup j "CopyTags") Boolean.of_json
  ; pre_signed_url = Aws.Util.option_map (Aws.Json.lookup j "PreSignedUrl") String.of_json
  ; option_group_name =
      Aws.Util.option_map (Aws.Json.lookup j "OptionGroupName") String.of_json
  ; target_custom_availability_zone =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetCustomAvailabilityZone")
        String.of_json
  ; snapshot_target =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotTarget") String.of_json
  ; copy_option_group =
      Aws.Util.option_map (Aws.Json.lookup j "CopyOptionGroup") Boolean.of_json
  ; snapshot_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "SnapshotAvailabilityZone") String.of_json
  }
