open Aws.BaseTypes

type t =
  { encrypted : Boolean.t option
  ; delete_on_termination : Boolean.t option
  ; iops : Integer.t option
  ; throughput : Integer.t option
  ; kms_key_id : String.t option
  ; snapshot_id : String.t option
  ; volume_size : Integer.t option
  ; volume_type : VolumeType.t option
  }

let make
    ?encrypted
    ?delete_on_termination
    ?iops
    ?throughput
    ?kms_key_id
    ?snapshot_id
    ?volume_size
    ?volume_type
    () =
  { encrypted
  ; delete_on_termination
  ; iops
  ; throughput
  ; kms_key_id
  ; snapshot_id
  ; volume_size
  ; volume_type
  }

let parse xml =
  Some
    { encrypted = Aws.Util.option_bind (Aws.Xml.member "encrypted" xml) Boolean.parse
    ; delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "deleteOnTermination" xml) Boolean.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "iops" xml) Integer.parse
    ; throughput = Aws.Util.option_bind (Aws.Xml.member "throughput" xml) Integer.parse
    ; kms_key_id = Aws.Util.option_bind (Aws.Xml.member "kmsKeyId" xml) String.parse
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; volume_size = Aws.Util.option_bind (Aws.Xml.member "volumeSize" xml) Integer.parse
    ; volume_type =
        Aws.Util.option_bind (Aws.Xml.member "volumeType" xml) VolumeType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_type (fun f ->
             Aws.Query.Pair ("VolumeType", VolumeType.to_query f))
       ; Aws.Util.option_map v.volume_size (fun f ->
             Aws.Query.Pair ("VolumeSize", Integer.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Aws.Util.option_map v.kms_key_id (fun f ->
             Aws.Query.Pair ("KmsKeyId", String.to_query f))
       ; Aws.Util.option_map v.throughput (fun f ->
             Aws.Query.Pair ("Throughput", Integer.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_type (fun f -> "volumeType", VolumeType.to_json f)
       ; Aws.Util.option_map v.volume_size (fun f -> "volumeSize", Integer.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ; Aws.Util.option_map v.kms_key_id (fun f -> "kmsKeyId", String.to_json f)
       ; Aws.Util.option_map v.throughput (fun f -> "throughput", Integer.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "iops", Integer.to_json f)
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             "deleteOnTermination", Boolean.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "encrypted", Boolean.to_json f)
       ])

let of_json j =
  { encrypted = Aws.Util.option_map (Aws.Json.lookup j "encrypted") Boolean.of_json
  ; delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "deleteOnTermination") Boolean.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "iops") Integer.of_json
  ; throughput = Aws.Util.option_map (Aws.Json.lookup j "throughput") Integer.of_json
  ; kms_key_id = Aws.Util.option_map (Aws.Json.lookup j "kmsKeyId") String.of_json
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; volume_size = Aws.Util.option_map (Aws.Json.lookup j "volumeSize") Integer.of_json
  ; volume_type = Aws.Util.option_map (Aws.Json.lookup j "volumeType") VolumeType.of_json
  }
