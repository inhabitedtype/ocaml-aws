open Aws.BaseTypes

type t = { stack_drift_detection_id : String.t }

let make ~stack_drift_detection_id () = { stack_drift_detection_id }

let parse xml =
  Some
    { stack_drift_detection_id =
        Aws.Xml.required
          "StackDriftDetectionId"
          (Aws.Util.option_bind (Aws.Xml.member "StackDriftDetectionId" xml) String.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("StackDriftDetectionId", String.to_query v.stack_drift_detection_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("StackDriftDetectionId", String.to_json v.stack_drift_detection_id) ])

let of_json j =
  { stack_drift_detection_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "StackDriftDetectionId"))
  }
