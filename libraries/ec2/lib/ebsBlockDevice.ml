open Aws.BaseTypes

type t =
  { delete_on_termination : Boolean.t option
  ; iops : Integer.t option
  ; snapshot_id : String.t option
  ; volume_size : Integer.t option
  ; volume_type : VolumeType.t option
  ; kms_key_id : String.t option
  ; throughput : Integer.t option
  ; outpost_arn : String.t option
  ; availability_zone : String.t option
  ; encrypted : Boolean.t option
  ; volume_initialization_rate : Integer.t option
  ; availability_zone_id : String.t option
  ; ebs_card_index : Integer.t option
  }

let make
    ?delete_on_termination
    ?iops
    ?snapshot_id
    ?volume_size
    ?volume_type
    ?kms_key_id
    ?throughput
    ?outpost_arn
    ?availability_zone
    ?encrypted
    ?volume_initialization_rate
    ?availability_zone_id
    ?ebs_card_index
    () =
  { delete_on_termination
  ; iops
  ; snapshot_id
  ; volume_size
  ; volume_type
  ; kms_key_id
  ; throughput
  ; outpost_arn
  ; availability_zone
  ; encrypted
  ; volume_initialization_rate
  ; availability_zone_id
  ; ebs_card_index
  }

let parse xml =
  Some
    { delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "deleteOnTermination" xml) Boolean.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "iops" xml) Integer.parse
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; volume_size = Aws.Util.option_bind (Aws.Xml.member "volumeSize" xml) Integer.parse
    ; volume_type =
        Aws.Util.option_bind (Aws.Xml.member "volumeType" xml) VolumeType.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "kmsKeyId" xml) String.parse
    ; throughput = Aws.Util.option_bind (Aws.Xml.member "throughput" xml) Integer.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "encrypted" xml) Boolean.parse
    ; volume_initialization_rate =
        Aws.Util.option_bind (Aws.Xml.member "VolumeInitializationRate" xml) Integer.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "AvailabilityZoneId" xml) String.parse
    ; ebs_card_index =
        Aws.Util.option_bind (Aws.Xml.member "EbsCardIndex" xml) Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ebs_card_index (fun f ->
             Aws.Query.Pair ("EbsCardIndex", Integer.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.volume_initialization_rate (fun f ->
             Aws.Query.Pair ("VolumeInitializationRate", Integer.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.throughput (fun f ->
             Aws.Query.Pair ("Throughput", Integer.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.volume_type (fun f ->
             Aws.Query.Pair ("VolumeType", VolumeType.to_query f))
       ; Aws.Util.option_map v.volume_size (fun f ->
             Aws.Query.Pair ("VolumeSize", Integer.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.ebs_card_index (fun f -> "EbsCardIndex", Integer.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "AvailabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.volume_initialization_rate (fun f ->
             "VolumeInitializationRate", Integer.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.throughput (fun f -> "throughput", Integer.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "kmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.volume_type (fun f -> "volumeType", VolumeType.to_json f)
       ; Aws.Util.option_map v.volume_size (fun f -> "volumeSize", Integer.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             "deleteOnTermination", Boolean.to_json f)
       ])

let of_json j =
  { delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "deleteOnTermination") Boolean.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "iops") Integer.of_json
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; volume_size = Aws.Util.option_map (Aws.Json.lookup j "volumeSize") Integer.of_json
  ; volume_type = Aws.Util.option_map (Aws.Json.lookup j "volumeType") VolumeType.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "kmsKeyId") String.of_json
  ; throughput = Aws.Util.option_map (Aws.Json.lookup j "throughput") Integer.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "encrypted") Boolean.of_json
  ; volume_initialization_rate =
      Aws.Util.option_map (Aws.Json.lookup j "VolumeInitializationRate") Integer.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "AvailabilityZoneId") String.of_json
  ; ebs_card_index =
      Aws.Util.option_map (Aws.Json.lookup j "EbsCardIndex") Integer.of_json
  }
