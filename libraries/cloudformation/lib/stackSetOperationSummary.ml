open Aws.BaseTypes

type t =
  { operation_id : String.t option
  ; action : StackSetOperationAction.t option
  ; status : StackSetOperationStatus.t option
  ; creation_timestamp : DateTime.t option
  ; end_timestamp : DateTime.t option
  ; status_reason : String.t option
  ; status_details : StackSetOperationStatusDetails.t option
  ; operation_preferences : StackSetOperationPreferences.t option
  }

let make
    ?operation_id
    ?action
    ?status
    ?creation_timestamp
    ?end_timestamp
    ?status_reason
    ?status_details
    ?operation_preferences
    () =
  { operation_id
  ; action
  ; status
  ; creation_timestamp
  ; end_timestamp
  ; status_reason
  ; status_details
  ; operation_preferences
  }

let parse xml =
  Some
    { operation_id = Aws.Util.option_bind (Aws.Xml.member "OperationId" xml) String.parse
    ; action =
        Aws.Util.option_bind (Aws.Xml.member "Action" xml) StackSetOperationAction.parse
    ; status =
        Aws.Util.option_bind (Aws.Xml.member "Status" xml) StackSetOperationStatus.parse
    ; creation_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "CreationTimestamp" xml) DateTime.parse
    ; end_timestamp =
        Aws.Util.option_bind (Aws.Xml.member "EndTimestamp" xml) DateTime.parse
    ; status_reason =
        Aws.Util.option_bind (Aws.Xml.member "StatusReason" xml) String.parse
    ; status_details =
        Aws.Util.option_bind
          (Aws.Xml.member "StatusDetails" xml)
          StackSetOperationStatusDetails.parse
    ; operation_preferences =
        Aws.Util.option_bind
          (Aws.Xml.member "OperationPreferences" xml)
          StackSetOperationPreferences.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operation_preferences (fun f ->
             Aws.Query.Pair
               ("OperationPreferences", StackSetOperationPreferences.to_query f))
       ; Aws.Util.option_map v.status_details (fun f ->
             Aws.Query.Pair ("StatusDetails", StackSetOperationStatusDetails.to_query f))
       ; Aws.Util.option_map v.status_reason (fun f ->
             Aws.Query.Pair ("StatusReason", String.to_query f))
       ; Aws.Util.option_map v.end_timestamp (fun f ->
             Aws.Query.Pair ("EndTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.creation_timestamp (fun f ->
             Aws.Query.Pair ("CreationTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", StackSetOperationStatus.to_query f))
       ; Aws.Util.option_map v.action (fun f ->
             Aws.Query.Pair ("Action", StackSetOperationAction.to_query f))
       ; Aws.Util.option_map v.operation_id (fun f ->
             Aws.Query.Pair ("OperationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operation_preferences (fun f ->
             "OperationPreferences", StackSetOperationPreferences.to_json f)
       ; Aws.Util.option_map v.status_details (fun f ->
             "StatusDetails", StackSetOperationStatusDetails.to_json f)
       ; Aws.Util.option_map v.status_reason (fun f -> "StatusReason", String.to_json f)
       ; Aws.Util.option_map v.end_timestamp (fun f -> "EndTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.creation_timestamp (fun f ->
             "CreationTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "Status", StackSetOperationStatus.to_json f)
       ; Aws.Util.option_map v.action (fun f ->
             "Action", StackSetOperationAction.to_json f)
       ; Aws.Util.option_map v.operation_id (fun f -> "OperationId", String.to_json f)
       ])

let of_json j =
  { operation_id = Aws.Util.option_map (Aws.Json.lookup j "OperationId") String.of_json
  ; action =
      Aws.Util.option_map (Aws.Json.lookup j "Action") StackSetOperationAction.of_json
  ; status =
      Aws.Util.option_map (Aws.Json.lookup j "Status") StackSetOperationStatus.of_json
  ; creation_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "CreationTimestamp") DateTime.of_json
  ; end_timestamp =
      Aws.Util.option_map (Aws.Json.lookup j "EndTimestamp") DateTime.of_json
  ; status_reason = Aws.Util.option_map (Aws.Json.lookup j "StatusReason") String.of_json
  ; status_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "StatusDetails")
        StackSetOperationStatusDetails.of_json
  ; operation_preferences =
      Aws.Util.option_map
        (Aws.Json.lookup j "OperationPreferences")
        StackSetOperationPreferences.of_json
  }
