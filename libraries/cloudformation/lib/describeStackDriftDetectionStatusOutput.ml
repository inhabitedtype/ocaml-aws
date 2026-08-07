open Aws.BaseTypes

type t =
  { stack_id : String.t
  ; stack_drift_detection_id : String.t
  ; stack_drift_status : StackDriftStatus.t option
  ; detection_status : StackDriftDetectionStatus.t
  ; detection_status_reason : String.t option
  ; drifted_stack_resource_count : Integer.t option
  ; timestamp : DateTime.t
  }

let make
    ~stack_id
    ~stack_drift_detection_id
    ?stack_drift_status
    ~detection_status
    ?detection_status_reason
    ?drifted_stack_resource_count
    ~timestamp
    () =
  { stack_id
  ; stack_drift_detection_id
  ; stack_drift_status
  ; detection_status
  ; detection_status_reason
  ; drifted_stack_resource_count
  ; timestamp
  }

let parse xml =
  Some
    { stack_id =
        Aws.Xml.required
          "StackId"
          (Aws.Util.option_bind (Aws.Xml.member "StackId" xml) String.parse)
    ; stack_drift_detection_id =
        Aws.Xml.required
          "StackDriftDetectionId"
          (Aws.Util.option_bind (Aws.Xml.member "StackDriftDetectionId" xml) String.parse)
    ; stack_drift_status =
        Aws.Util.option_bind
          (Aws.Xml.member "StackDriftStatus" xml)
          StackDriftStatus.parse
    ; detection_status =
        Aws.Xml.required
          "DetectionStatus"
          (Aws.Util.option_bind
             (Aws.Xml.member "DetectionStatus" xml)
             StackDriftDetectionStatus.parse)
    ; detection_status_reason =
        Aws.Util.option_bind (Aws.Xml.member "DetectionStatusReason" xml) String.parse
    ; drifted_stack_resource_count =
        Aws.Util.option_bind
          (Aws.Xml.member "DriftedStackResourceCount" xml)
          Integer.parse
    ; timestamp =
        Aws.Xml.required
          "Timestamp"
          (Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("Timestamp", DateTime.to_query v.timestamp))
       ; Aws.Util.option_map v.drifted_stack_resource_count (fun f ->
             Aws.Query.Pair ("DriftedStackResourceCount", Integer.to_query f))
       ; Aws.Util.option_map v.detection_status_reason (fun f ->
             Aws.Query.Pair ("DetectionStatusReason", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("DetectionStatus", StackDriftDetectionStatus.to_query v.detection_status))
       ; Aws.Util.option_map v.stack_drift_status (fun f ->
             Aws.Query.Pair ("StackDriftStatus", StackDriftStatus.to_query f))
       ; Some
           (Aws.Query.Pair
              ("StackDriftDetectionId", String.to_query v.stack_drift_detection_id))
       ; Some (Aws.Query.Pair ("StackId", String.to_query v.stack_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("Timestamp", DateTime.to_json v.timestamp)
       ; Aws.Util.option_map v.drifted_stack_resource_count (fun f ->
             "DriftedStackResourceCount", Integer.to_json f)
       ; Aws.Util.option_map v.detection_status_reason (fun f ->
             "DetectionStatusReason", String.to_json f)
       ; Some ("DetectionStatus", StackDriftDetectionStatus.to_json v.detection_status)
       ; Aws.Util.option_map v.stack_drift_status (fun f ->
             "StackDriftStatus", StackDriftStatus.to_json f)
       ; Some ("StackDriftDetectionId", String.to_json v.stack_drift_detection_id)
       ; Some ("StackId", String.to_json v.stack_id)
       ])

let of_json j =
  { stack_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackId"))
  ; stack_drift_detection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackDriftDetectionId"))
  ; stack_drift_status =
      Aws.Util.option_map (Aws.Json.lookup j "StackDriftStatus") StackDriftStatus.of_json
  ; detection_status =
      StackDriftDetectionStatus.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "DetectionStatus"))
  ; detection_status_reason =
      Aws.Util.option_map (Aws.Json.lookup j "DetectionStatusReason") String.of_json
  ; drifted_stack_resource_count =
      Aws.Util.option_map (Aws.Json.lookup j "DriftedStackResourceCount") Integer.of_json
  ; timestamp = DateTime.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "Timestamp"))
  }
