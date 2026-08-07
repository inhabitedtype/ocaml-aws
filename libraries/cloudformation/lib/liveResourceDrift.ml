open Aws.BaseTypes

type t =
  { previous_value : String.t option
  ; actual_value : String.t option
  ; drift_detection_timestamp : DateTime.t option
  }

let make ?previous_value ?actual_value ?drift_detection_timestamp () =
  { previous_value; actual_value; drift_detection_timestamp }

let parse xml =
  Some
    { previous_value =
        Aws.Util.option_bind (Aws.Xml.member "PreviousValue" xml) String.parse
    ; actual_value = Aws.Util.option_bind (Aws.Xml.member "ActualValue" xml) String.parse
    ; drift_detection_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "DriftDetectionTimestamp" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.drift_detection_timestamp (fun f ->
             Aws.Query.Pair ("DriftDetectionTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.actual_value (fun f ->
             Aws.Query.Pair ("ActualValue", String.to_query f))
       ; Aws.Util.option_map v.previous_value (fun f ->
             Aws.Query.Pair ("PreviousValue", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.drift_detection_timestamp (fun f ->
             "DriftDetectionTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.actual_value (fun f -> "ActualValue", String.to_json f)
       ; Aws.Util.option_map v.previous_value (fun f -> "PreviousValue", String.to_json f)
       ])

let of_json j =
  { previous_value =
      Aws.Util.option_map (Aws.Json.lookup j "PreviousValue") String.of_json
  ; actual_value = Aws.Util.option_map (Aws.Json.lookup j "ActualValue") String.of_json
  ; drift_detection_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "DriftDetectionTimestamp") DateTime.of_json
  }
