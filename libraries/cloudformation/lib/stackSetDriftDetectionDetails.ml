open Aws.BaseTypes

type t =
  { drift_status : StackSetDriftStatus.t option
  ; drift_detection_status : StackSetDriftDetectionStatus.t option
  ; last_drift_check_timestamp : DateTime.t option
  ; total_stack_instances_count : Integer.t option
  ; drifted_stack_instances_count : Integer.t option
  ; in_sync_stack_instances_count : Integer.t option
  ; in_progress_stack_instances_count : Integer.t option
  ; failed_stack_instances_count : Integer.t option
  }

let make
    ?drift_status
    ?drift_detection_status
    ?last_drift_check_timestamp
    ?total_stack_instances_count
    ?drifted_stack_instances_count
    ?in_sync_stack_instances_count
    ?in_progress_stack_instances_count
    ?failed_stack_instances_count
    () =
  { drift_status
  ; drift_detection_status
  ; last_drift_check_timestamp
  ; total_stack_instances_count
  ; drifted_stack_instances_count
  ; in_sync_stack_instances_count
  ; in_progress_stack_instances_count
  ; failed_stack_instances_count
  }

let parse xml =
  Some
    { drift_status =
        Aws.Util.option_bind (Aws.Xml.member "DriftStatus" xml) StackSetDriftStatus.parse
    ; drift_detection_status =
        Aws.Util.option_bind
          (Aws.Xml.member "DriftDetectionStatus" xml)
          StackSetDriftDetectionStatus.parse
    ; last_drift_check_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "LastDriftCheckTimestamp" xml) DateTime.parse
    ; total_stack_instances_count =
        Aws.Util.option_bind (Aws.Xml.member "TotalStackInstancesCount" xml) Integer.parse
    ; drifted_stack_instances_count =
        Aws.Util.option_bind
          (Aws.Xml.member "DriftedStackInstancesCount" xml)
          Integer.parse
    ; in_sync_stack_instances_count =
        Aws.Util.option_bind
          (Aws.Xml.member "InSyncStackInstancesCount" xml)
          Integer.parse
    ; in_progress_stack_instances_count =
        Aws.Util.option_bind
          (Aws.Xml.member "InProgressStackInstancesCount" xml)
          Integer.parse
    ; failed_stack_instances_count =
        Aws.Util.option_bind
          (Aws.Xml.member "FailedStackInstancesCount" xml)
          Integer.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failed_stack_instances_count (fun f ->
             Aws.Query.Pair ("FailedStackInstancesCount", Integer.to_query f))
       ; Aws.Util.option_map v.in_progress_stack_instances_count (fun f ->
             Aws.Query.Pair ("InProgressStackInstancesCount", Integer.to_query f))
       ; Aws.Util.option_map v.in_sync_stack_instances_count (fun f ->
             Aws.Query.Pair ("InSyncStackInstancesCount", Integer.to_query f))
       ; Aws.Util.option_map v.drifted_stack_instances_count (fun f ->
             Aws.Query.Pair ("DriftedStackInstancesCount", Integer.to_query f))
       ; Aws.Util.option_map v.total_stack_instances_count (fun f ->
             Aws.Query.Pair ("TotalStackInstancesCount", Integer.to_query f))
       ; Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
             Aws.Query.Pair ("LastDriftCheckTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.drift_detection_status (fun f ->
             Aws.Query.Pair
               ("DriftDetectionStatus", StackSetDriftDetectionStatus.to_query f))
       ; Aws.Util.option_map v.drift_status (fun f ->
             Aws.Query.Pair ("DriftStatus", StackSetDriftStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.failed_stack_instances_count (fun f ->
             "FailedStackInstancesCount", Integer.to_json f)
       ; Aws.Util.option_map v.in_progress_stack_instances_count (fun f ->
             "InProgressStackInstancesCount", Integer.to_json f)
       ; Aws.Util.option_map v.in_sync_stack_instances_count (fun f ->
             "InSyncStackInstancesCount", Integer.to_json f)
       ; Aws.Util.option_map v.drifted_stack_instances_count (fun f ->
             "DriftedStackInstancesCount", Integer.to_json f)
       ; Aws.Util.option_map v.total_stack_instances_count (fun f ->
             "TotalStackInstancesCount", Integer.to_json f)
       ; Aws.Util.option_map v.last_drift_check_timestamp (fun f ->
             "LastDriftCheckTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.drift_detection_status (fun f ->
             "DriftDetectionStatus", StackSetDriftDetectionStatus.to_json f)
       ; Aws.Util.option_map v.drift_status (fun f ->
             "DriftStatus", StackSetDriftStatus.to_json f)
       ])

let of_json j =
  { drift_status =
      Aws.Util.option_map (Aws.Json.lookup j "DriftStatus") StackSetDriftStatus.of_json
  ; drift_detection_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "DriftDetectionStatus")
        StackSetDriftDetectionStatus.of_json
  ; last_drift_check_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "LastDriftCheckTimestamp") DateTime.of_json
  ; total_stack_instances_count =
      Aws.Util.option_map (Aws.Json.lookup j "TotalStackInstancesCount") Integer.of_json
  ; drifted_stack_instances_count =
      Aws.Util.option_map (Aws.Json.lookup j "DriftedStackInstancesCount") Integer.of_json
  ; in_sync_stack_instances_count =
      Aws.Util.option_map (Aws.Json.lookup j "InSyncStackInstancesCount") Integer.of_json
  ; in_progress_stack_instances_count =
      Aws.Util.option_map
        (Aws.Json.lookup j "InProgressStackInstancesCount")
        Integer.of_json
  ; failed_stack_instances_count =
      Aws.Util.option_map (Aws.Json.lookup j "FailedStackInstancesCount") Integer.of_json
  }
