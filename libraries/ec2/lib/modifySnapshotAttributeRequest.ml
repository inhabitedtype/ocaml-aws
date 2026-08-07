open Aws.BaseTypes

type t =
  { attribute : SnapshotAttributeName.t option
  ; create_volume_permission : CreateVolumePermissionModifications.t option
  ; group_names : GroupNameStringList.t
  ; operation_type : OperationType.t option
  ; snapshot_id : String.t
  ; user_ids : UserIdStringList.t
  ; dry_run : Boolean.t option
  }

let make
    ?attribute
    ?create_volume_permission
    ?(group_names = [])
    ?operation_type
    ~snapshot_id
    ?(user_ids = [])
    ?dry_run
    () =
  { attribute
  ; create_volume_permission
  ; group_names
  ; operation_type
  ; snapshot_id
  ; user_ids
  ; dry_run
  }

let parse xml =
  Some
    { attribute =
        Aws.Util.option_bind (Aws.Xml.member "Attribute" xml) SnapshotAttributeName.parse
    ; create_volume_permission =
        Aws.Util.option_bind
          (Aws.Xml.member "CreateVolumePermission" xml)
          CreateVolumePermissionModifications.parse
    ; group_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "UserGroup" xml)
             GroupNameStringList.parse)
    ; operation_type =
        Aws.Util.option_bind (Aws.Xml.member "OperationType" xml) OperationType.parse
    ; snapshot_id =
        Aws.Xml.required
          "SnapshotId"
          (Aws.Util.option_bind (Aws.Xml.member "SnapshotId" xml) String.parse)
    ; user_ids =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "UserId" xml) UserIdStringList.parse)
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Some (Aws.Query.Pair ("UserId", UserIdStringList.to_query v.user_ids))
       ; Some (Aws.Query.Pair ("SnapshotId", String.to_query v.snapshot_id))
       ; Aws.Util.option_map v.operation_type (fun f ->
             Aws.Query.Pair ("OperationType", OperationType.to_query f))
       ; Some (Aws.Query.Pair ("UserGroup", GroupNameStringList.to_query v.group_names))
       ; Aws.Util.option_map v.create_volume_permission (fun f ->
             Aws.Query.Pair
               ("CreateVolumePermission", CreateVolumePermissionModifications.to_query f))
       ; Aws.Util.option_map v.attribute (fun f ->
             Aws.Query.Pair ("Attribute", SnapshotAttributeName.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Some ("UserId", UserIdStringList.to_json v.user_ids)
       ; Some ("SnapshotId", String.to_json v.snapshot_id)
       ; Aws.Util.option_map v.operation_type (fun f ->
             "OperationType", OperationType.to_json f)
       ; Some ("UserGroup", GroupNameStringList.to_json v.group_names)
       ; Aws.Util.option_map v.create_volume_permission (fun f ->
             "CreateVolumePermission", CreateVolumePermissionModifications.to_json f)
       ; Aws.Util.option_map v.attribute (fun f ->
             "Attribute", SnapshotAttributeName.to_json f)
       ])

let of_json j =
  { attribute =
      Aws.Util.option_map (Aws.Json.lookup j "Attribute") SnapshotAttributeName.of_json
  ; create_volume_permission =
      Aws.Util.option_map
        (Aws.Json.lookup j "CreateVolumePermission")
        CreateVolumePermissionModifications.of_json
  ; group_names =
      GroupNameStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserGroup"))
  ; operation_type =
      Aws.Util.option_map (Aws.Json.lookup j "OperationType") OperationType.of_json
  ; snapshot_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "SnapshotId"))
  ; user_ids =
      UserIdStringList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "UserId"))
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
