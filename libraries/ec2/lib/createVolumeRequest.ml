open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; encrypted : Boolean.t option
  ; iops : Integer.t option
  ; kms_key_id : String.t option
  ; outpost_arn : String.t option
  ; size : Integer.t option
  ; snapshot_id : String.t option
  ; volume_type : VolumeType.t option
  ; tag_specifications : TagSpecificationList.t
  ; multi_attach_enabled : Boolean.t option
  ; throughput : Integer.t option
  ; client_token : String.t option
  ; volume_initialization_rate : Integer.t option
  ; operator : OperatorRequest.t option
  ; dry_run : Boolean.t option
  }

let make
    ?availability_zone
    ?availability_zone_id
    ?encrypted
    ?iops
    ?kms_key_id
    ?outpost_arn
    ?size
    ?snapshot_id
    ?volume_type
    ?(tag_specifications = [])
    ?multi_attach_enabled
    ?throughput
    ?client_token
    ?volume_initialization_rate
    ?operator
    ?dry_run
    () =
  { availability_zone
  ; availability_zone_id
  ; encrypted
  ; iops
  ; kms_key_id
  ; outpost_arn
  ; size
  ; snapshot_id
  ; volume_type
  ; tag_specifications
  ; multi_attach_enabled
  ; throughput
  ; client_token
  ; volume_initialization_rate
  ; operator
  ; dry_run
  }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "encrypted" xml) Boolean.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "KmsKeyId" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "OutpostArn" xml) String.parse
    ; size = Aws.Util.option_bind (Aws.Xml.member "Size" xml) Integer.parse
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "SnapshotId" xml) String.parse
    ; volume_type =
        Aws.Util.option_bind (Aws.Xml.member "VolumeType" xml) VolumeType.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; multi_attach_enabled =
        Aws.Util.option_bind (Aws.Xml.member "MultiAttachEnabled" xml) Boolean.parse
    ; throughput = Aws.Util.option_bind (Aws.Xml.member "Throughput" xml) Integer.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; volume_initialization_rate =
        Aws.Util.option_bind (Aws.Xml.member "VolumeInitializationRate" xml) Integer.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "Operator" xml) OperatorRequest.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "dryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorRequest.to_query f))
       ; Aws.Util.option_map v.volume_initialization_rate (fun f ->
             Aws.Query.Pair ("VolumeInitializationRate", Integer.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.throughput (fun f ->
             Aws.Query.Pair ("Throughput", Integer.to_query f))
       ; Aws.Util.option_map v.multi_attach_enabled (fun f ->
             Aws.Query.Pair ("MultiAttachEnabled", Boolean.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.volume_type (fun f ->
             Aws.Query.Pair ("VolumeType", VolumeType.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Aws.Util.option_map v.size (fun f -> Aws.Query.Pair ("Size", Integer.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "dryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.operator (fun f -> "Operator", OperatorRequest.to_json f)
       ; Aws.Util.option_map v.volume_initialization_rate (fun f ->
             "VolumeInitializationRate", Integer.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.throughput (fun f -> "Throughput", Integer.to_json f)
       ; Aws.Util.option_map v.multi_attach_enabled (fun f ->
             "MultiAttachEnabled", Boolean.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.volume_type (fun f -> "VolumeType", VolumeType.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "SnapshotId", String.to_json f)
       ; Aws.Util.option_map v.size (fun f -> "Size", Integer.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "OutpostArn", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "KmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "AvailabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "encrypted") Boolean.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "KmsKeyId") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "OutpostArn") String.of_json
  ; size = Aws.Util.option_map (Aws.Json.lookup j "Size") Integer.of_json
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "SnapshotId") String.of_json
  ; volume_type = Aws.Util.option_map (Aws.Json.lookup j "VolumeType") VolumeType.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; multi_attach_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "MultiAttachEnabled") Boolean.of_json
  ; throughput = Aws.Util.option_map (Aws.Json.lookup j "Throughput") Integer.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; volume_initialization_rate =
      Aws.Util.option_map (Aws.Json.lookup j "VolumeInitializationRate") Integer.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "Operator") OperatorRequest.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "dryRun") Boolean.of_json
  }
