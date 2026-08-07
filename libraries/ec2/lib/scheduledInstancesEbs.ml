open Aws.BaseTypes

type t =
  { delete_on_termination : Boolean.t option
  ; encrypted : Boolean.t option
  ; iops : Integer.t option
  ; snapshot_id : String.t option
  ; volume_size : Integer.t option
  ; volume_type : String.t option
  }

let make ?delete_on_termination ?encrypted ?iops ?snapshot_id ?volume_size ?volume_type ()
    =
  { delete_on_termination; encrypted; iops; snapshot_id; volume_size; volume_type }

let parse xml =
  Some
    { delete_on_termination =
        Aws.Util.option_bind (Aws.Xml.member "DeleteOnTermination" xml) Boolean.parse
    ; encrypted = Aws.Util.option_bind (Aws.Xml.member "Encrypted" xml) Boolean.parse
    ; iops = Aws.Util.option_bind (Aws.Xml.member "Iops" xml) Integer.parse
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "SnapshotId" xml) String.parse
    ; volume_size = Aws.Util.option_bind (Aws.Xml.member "VolumeSize" xml) Integer.parse
    ; volume_type = Aws.Util.option_bind (Aws.Xml.member "VolumeType" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_type (fun f ->
             Aws.Query.Pair ("VolumeType", String.to_query f))
       ; Aws.Util.option_map v.volume_size (fun f ->
             Aws.Query.Pair ("VolumeSize", Integer.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Aws.Util.option_map v.iops (fun f -> Aws.Query.Pair ("Iops", Integer.to_query f))
       ; Aws.Util.option_map v.encrypted (fun f ->
             Aws.Query.Pair ("Encrypted", Boolean.to_query f))
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             Aws.Query.Pair ("DeleteOnTermination", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.volume_type (fun f -> "VolumeType", String.to_json f)
       ; Aws.Util.option_map v.volume_size (fun f -> "VolumeSize", Integer.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "SnapshotId", String.to_json f)
       ; Aws.Util.option_map v.iops (fun f -> "Iops", Integer.to_json f)
       ; Aws.Util.option_map v.encrypted (fun f -> "Encrypted", Boolean.to_json f)
       ; Aws.Util.option_map v.delete_on_termination (fun f ->
             "DeleteOnTermination", Boolean.to_json f)
       ])

let of_json j =
  { delete_on_termination =
      Aws.Util.option_map (Aws.Json.lookup j "DeleteOnTermination") Boolean.of_json
  ; encrypted = Aws.Util.option_map (Aws.Json.lookup j "Encrypted") Boolean.of_json
  ; iops = Aws.Util.option_map (Aws.Json.lookup j "Iops") Integer.of_json
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "SnapshotId") String.of_json
  ; volume_size = Aws.Util.option_map (Aws.Json.lookup j "VolumeSize") Integer.of_json
  ; volume_type = Aws.Util.option_map (Aws.Json.lookup j "VolumeType") String.of_json
  }
