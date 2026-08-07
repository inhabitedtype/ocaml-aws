open Aws.BaseTypes

type t =
  { volume_id : String.t option
  ; modification_state : VolumeModificationState.t option
  ; status_message : String.t option
  ; target_size : Integer.t option
  ; target_iops : Integer.t option
  ; target_volume_type : VolumeType.t option
  ; target_throughput : Integer.t option
  ; target_multi_attach_enabled : Boolean.t option
  ; original_size : Integer.t option
  ; original_iops : Integer.t option
  ; original_volume_type : VolumeType.t option
  ; original_throughput : Integer.t option
  ; original_multi_attach_enabled : Boolean.t option
  ; progress : Long.t option
  ; start_time : DateTime.t option
  ; end_time : DateTime.t option
  }

let make
    ?volume_id
    ?modification_state
    ?status_message
    ?target_size
    ?target_iops
    ?target_volume_type
    ?target_throughput
    ?target_multi_attach_enabled
    ?original_size
    ?original_iops
    ?original_volume_type
    ?original_throughput
    ?original_multi_attach_enabled
    ?progress
    ?start_time
    ?end_time
    () =
  { volume_id
  ; modification_state
  ; status_message
  ; target_size
  ; target_iops
  ; target_volume_type
  ; target_throughput
  ; target_multi_attach_enabled
  ; original_size
  ; original_iops
  ; original_volume_type
  ; original_throughput
  ; original_multi_attach_enabled
  ; progress
  ; start_time
  ; end_time
  }

let parse xml =
  Some
    { volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    ; modification_state =
        Aws.Util.option_bind
          (Aws.Xml.member "modificationState" xml)
          VolumeModificationState.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; target_size = Aws.Util.option_bind (Aws.Xml.member "targetSize" xml) Integer.parse
    ; target_iops = Aws.Util.option_bind (Aws.Xml.member "targetIops" xml) Integer.parse
    ; target_volume_type =
        Aws.Util.option_bind (Aws.Xml.member "targetVolumeType" xml) VolumeType.parse
    ; target_throughput =
        Aws.Util.option_bind (Aws.Xml.member "targetThroughput" xml) Integer.parse
    ; target_multi_attach_enabled =
        Aws.Util.option_bind (Aws.Xml.member "targetMultiAttachEnabled" xml) Boolean.parse
    ; original_size =
        Aws.Util.option_bind (Aws.Xml.member "originalSize" xml) Integer.parse
    ; original_iops =
        Aws.Util.option_bind (Aws.Xml.member "originalIops" xml) Integer.parse
    ; original_volume_type =
        Aws.Util.option_bind (Aws.Xml.member "originalVolumeType" xml) VolumeType.parse
    ; original_throughput =
        Aws.Util.option_bind (Aws.Xml.member "originalThroughput" xml) Integer.parse
    ; original_multi_attach_enabled =
        Aws.Util.option_bind
          (Aws.Xml.member "originalMultiAttachEnabled" xml)
          Boolean.parse
    ; progress = Aws.Util.option_bind (Aws.Xml.member "progress" xml) Long.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "startTime" xml) DateTime.parse
    ; end_time = Aws.Util.option_bind (Aws.Xml.member "endTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.end_time (fun f ->
             Aws.Query.Pair ("EndTime", DateTime.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", DateTime.to_query f))
       ; Aws.Util.option_map v.progress (fun f ->
             Aws.Query.Pair ("Progress", Long.to_query f))
       ; Aws.Util.option_map v.original_multi_attach_enabled (fun f ->
             Aws.Query.Pair ("OriginalMultiAttachEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.original_throughput (fun f ->
             Aws.Query.Pair ("OriginalThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.original_volume_type (fun f ->
             Aws.Query.Pair ("OriginalVolumeType", VolumeType.to_query f))
       ; Aws.Util.option_map v.original_iops (fun f ->
             Aws.Query.Pair ("OriginalIops", Integer.to_query f))
       ; Aws.Util.option_map v.original_size (fun f ->
             Aws.Query.Pair ("OriginalSize", Integer.to_query f))
       ; Aws.Util.option_map v.target_multi_attach_enabled (fun f ->
             Aws.Query.Pair ("TargetMultiAttachEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.target_throughput (fun f ->
             Aws.Query.Pair ("TargetThroughput", Integer.to_query f))
       ; Aws.Util.option_map v.target_volume_type (fun f ->
             Aws.Query.Pair ("TargetVolumeType", VolumeType.to_query f))
       ; Aws.Util.option_map v.target_iops (fun f ->
             Aws.Query.Pair ("TargetIops", Integer.to_query f))
       ; Aws.Util.option_map v.target_size (fun f ->
             Aws.Query.Pair ("TargetSize", Integer.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.modification_state (fun f ->
             Aws.Query.Pair ("ModificationState", VolumeModificationState.to_query f))
       ; Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.end_time (fun f -> "endTime", DateTime.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "startTime", DateTime.to_json f)
       ; Aws.Util.option_map v.progress (fun f -> "progress", Long.to_json f)
       ; Aws.Util.option_map v.original_multi_attach_enabled (fun f ->
             "originalMultiAttachEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.original_throughput (fun f ->
             "originalThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.original_volume_type (fun f ->
             "originalVolumeType", VolumeType.to_json f)
       ; Aws.Util.option_map v.original_iops (fun f -> "originalIops", Integer.to_json f)
       ; Aws.Util.option_map v.original_size (fun f -> "originalSize", Integer.to_json f)
       ; Aws.Util.option_map v.target_multi_attach_enabled (fun f ->
             "targetMultiAttachEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.target_throughput (fun f ->
             "targetThroughput", Integer.to_json f)
       ; Aws.Util.option_map v.target_volume_type (fun f ->
             "targetVolumeType", VolumeType.to_json f)
       ; Aws.Util.option_map v.target_iops (fun f -> "targetIops", Integer.to_json f)
       ; Aws.Util.option_map v.target_size (fun f -> "targetSize", Integer.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.modification_state (fun f ->
             "modificationState", VolumeModificationState.to_json f)
       ; Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ])

let of_json j =
  { volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  ; modification_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "modificationState")
        VolumeModificationState.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; target_size = Aws.Util.option_map (Aws.Json.lookup j "targetSize") Integer.of_json
  ; target_iops = Aws.Util.option_map (Aws.Json.lookup j "targetIops") Integer.of_json
  ; target_volume_type =
      Aws.Util.option_map (Aws.Json.lookup j "targetVolumeType") VolumeType.of_json
  ; target_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "targetThroughput") Integer.of_json
  ; target_multi_attach_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "targetMultiAttachEnabled") Boolean.of_json
  ; original_size = Aws.Util.option_map (Aws.Json.lookup j "originalSize") Integer.of_json
  ; original_iops = Aws.Util.option_map (Aws.Json.lookup j "originalIops") Integer.of_json
  ; original_volume_type =
      Aws.Util.option_map (Aws.Json.lookup j "originalVolumeType") VolumeType.of_json
  ; original_throughput =
      Aws.Util.option_map (Aws.Json.lookup j "originalThroughput") Integer.of_json
  ; original_multi_attach_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "originalMultiAttachEnabled") Boolean.of_json
  ; progress = Aws.Util.option_map (Aws.Json.lookup j "progress") Long.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "startTime") DateTime.of_json
  ; end_time = Aws.Util.option_map (Aws.Json.lookup j "endTime") DateTime.of_json
  }
