open Aws.BaseTypes

type t =
  { replace_root_volume_task_id : String.t option
  ; instance_id : String.t option
  ; task_state : ReplaceRootVolumeTaskState.t option
  ; start_time : String.t option
  ; complete_time : String.t option
  ; tags : TagList.t
  ; image_id : String.t option
  ; snapshot_id : String.t option
  ; delete_replaced_root_volume : Boolean.t option
  }

let make
    ?replace_root_volume_task_id
    ?instance_id
    ?task_state
    ?start_time
    ?complete_time
    ?(tags = [])
    ?image_id
    ?snapshot_id
    ?delete_replaced_root_volume
    () =
  { replace_root_volume_task_id
  ; instance_id
  ; task_state
  ; start_time
  ; complete_time
  ; tags
  ; image_id
  ; snapshot_id
  ; delete_replaced_root_volume
  }

let parse xml =
  Some
    { replace_root_volume_task_id =
        Aws.Util.option_bind (Aws.Xml.member "replaceRootVolumeTaskId" xml) String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; task_state =
        Aws.Util.option_bind
          (Aws.Xml.member "taskState" xml)
          ReplaceRootVolumeTaskState.parse
    ; start_time = Aws.Util.option_bind (Aws.Xml.member "startTime" xml) String.parse
    ; complete_time =
        Aws.Util.option_bind (Aws.Xml.member "completeTime" xml) String.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; image_id = Aws.Util.option_bind (Aws.Xml.member "imageId" xml) String.parse
    ; snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; delete_replaced_root_volume =
        Aws.Util.option_bind (Aws.Xml.member "deleteReplacedRootVolume" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delete_replaced_root_volume (fun f ->
             Aws.Query.Pair ("DeleteReplacedRootVolume", Boolean.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ; Aws.Util.option_map v.image_id (fun f ->
             Aws.Query.Pair ("ImageId", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.complete_time (fun f ->
             Aws.Query.Pair ("CompleteTime", String.to_query f))
       ; Aws.Util.option_map v.start_time (fun f ->
             Aws.Query.Pair ("StartTime", String.to_query f))
       ; Aws.Util.option_map v.task_state (fun f ->
             Aws.Query.Pair ("TaskState", ReplaceRootVolumeTaskState.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.replace_root_volume_task_id (fun f ->
             Aws.Query.Pair ("ReplaceRootVolumeTaskId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.delete_replaced_root_volume (fun f ->
             "deleteReplacedRootVolume", Boolean.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ; Aws.Util.option_map v.image_id (fun f -> "imageId", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.complete_time (fun f -> "completeTime", String.to_json f)
       ; Aws.Util.option_map v.start_time (fun f -> "startTime", String.to_json f)
       ; Aws.Util.option_map v.task_state (fun f ->
             "taskState", ReplaceRootVolumeTaskState.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Aws.Util.option_map v.replace_root_volume_task_id (fun f ->
             "replaceRootVolumeTaskId", String.to_json f)
       ])

let of_json j =
  { replace_root_volume_task_id =
      Aws.Util.option_map (Aws.Json.lookup j "replaceRootVolumeTaskId") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; task_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "taskState")
        ReplaceRootVolumeTaskState.of_json
  ; start_time = Aws.Util.option_map (Aws.Json.lookup j "startTime") String.of_json
  ; complete_time = Aws.Util.option_map (Aws.Json.lookup j "completeTime") String.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; image_id = Aws.Util.option_map (Aws.Json.lookup j "imageId") String.of_json
  ; snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; delete_replaced_root_volume =
      Aws.Util.option_map (Aws.Json.lookup j "deleteReplacedRootVolume") Boolean.of_json
  }
