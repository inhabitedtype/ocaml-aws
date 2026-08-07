open Aws.BaseTypes

type t =
  { operation : KeyLastUsageTrackingOperation.t option
  ; timestamp : DateTime.t option
  ; cloud_trail_event_id : String.t option
  ; kms_request_id : String.t option
  }

let make ?operation ?timestamp ?cloud_trail_event_id ?kms_request_id () =
  { operation; timestamp; cloud_trail_event_id; kms_request_id }

let parse xml =
  Some
    { operation =
        Aws.Util.option_bind
          (Aws.Xml.member "Operation" xml)
          KeyLastUsageTrackingOperation.parse
    ; timestamp = Aws.Util.option_bind (Aws.Xml.member "Timestamp" xml) DateTime.parse
    ; cloud_trail_event_id =
        Aws.Util.option_bind (Aws.Xml.member "CloudTrailEventId" xml) String.parse
    ; kms_request_id =
        Aws.Util.option_bind (Aws.Xml.member "KmsRequestId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_request_id (fun f ->
             Aws.Query.Pair ("KmsRequestId", String.to_query f))
       ; Aws.Util.option_map v.cloud_trail_event_id (fun f ->
             Aws.Query.Pair ("CloudTrailEventId", String.to_query f))
       ; Aws.Util.option_map v.timestamp (fun f ->
             Aws.Query.Pair ("Timestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.operation (fun f ->
             Aws.Query.Pair ("Operation", KeyLastUsageTrackingOperation.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.kms_request_id (fun f -> "KmsRequestId", String.to_json f)
       ; Aws.Util.option_map v.cloud_trail_event_id (fun f ->
             "CloudTrailEventId", String.to_json f)
       ; Aws.Util.option_map v.timestamp (fun f -> "Timestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.operation (fun f ->
             "Operation", KeyLastUsageTrackingOperation.to_json f)
       ])

let of_json j =
  { operation =
      Aws.Util.option_map
        (Aws.Json.lookup j "Operation")
        KeyLastUsageTrackingOperation.of_json
  ; timestamp = Aws.Util.option_map (Aws.Json.lookup j "Timestamp") DateTime.of_json
  ; cloud_trail_event_id =
      Aws.Util.option_map (Aws.Json.lookup j "CloudTrailEventId") String.of_json
  ; kms_request_id = Aws.Util.option_map (Aws.Json.lookup j "KmsRequestId") String.of_json
  }
