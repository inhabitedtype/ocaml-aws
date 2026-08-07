open Aws.BaseTypes

type t =
  { tag_key : String.t option
  ; status : CapacityManagerMonitoredTagKeyStatus.t option
  ; status_message : String.t option
  ; capacity_manager_provided : Boolean.t option
  ; earliest_datapoint_timestamp : DateTime.t option
  }

let make
    ?tag_key
    ?status
    ?status_message
    ?capacity_manager_provided
    ?earliest_datapoint_timestamp
    () =
  { tag_key
  ; status
  ; status_message
  ; capacity_manager_provided
  ; earliest_datapoint_timestamp
  }

let parse xml =
  Some
    { tag_key = Aws.Util.option_bind (Aws.Xml.member "tagKey" xml) String.parse
    ; status =
        Aws.Util.option_bind
          (Aws.Xml.member "status" xml)
          CapacityManagerMonitoredTagKeyStatus.parse
    ; status_message =
        Aws.Util.option_bind (Aws.Xml.member "statusMessage" xml) String.parse
    ; capacity_manager_provided =
        Aws.Util.option_bind (Aws.Xml.member "capacityManagerProvided" xml) Boolean.parse
    ; earliest_datapoint_timestamp =
        Aws.Util.option_bind
          (Aws.Xml.member "earliestDatapointTimestamp" xml)
          DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.earliest_datapoint_timestamp (fun f ->
             Aws.Query.Pair ("EarliestDatapointTimestamp", DateTime.to_query f))
       ; Aws.Util.option_map v.capacity_manager_provided (fun f ->
             Aws.Query.Pair ("CapacityManagerProvided", Boolean.to_query f))
       ; Aws.Util.option_map v.status_message (fun f ->
             Aws.Query.Pair ("StatusMessage", String.to_query f))
       ; Aws.Util.option_map v.status (fun f ->
             Aws.Query.Pair ("Status", CapacityManagerMonitoredTagKeyStatus.to_query f))
       ; Aws.Util.option_map v.tag_key (fun f ->
             Aws.Query.Pair ("TagKey", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.earliest_datapoint_timestamp (fun f ->
             "earliestDatapointTimestamp", DateTime.to_json f)
       ; Aws.Util.option_map v.capacity_manager_provided (fun f ->
             "capacityManagerProvided", Boolean.to_json f)
       ; Aws.Util.option_map v.status_message (fun f -> "statusMessage", String.to_json f)
       ; Aws.Util.option_map v.status (fun f ->
             "status", CapacityManagerMonitoredTagKeyStatus.to_json f)
       ; Aws.Util.option_map v.tag_key (fun f -> "tagKey", String.to_json f)
       ])

let of_json j =
  { tag_key = Aws.Util.option_map (Aws.Json.lookup j "tagKey") String.of_json
  ; status =
      Aws.Util.option_map
        (Aws.Json.lookup j "status")
        CapacityManagerMonitoredTagKeyStatus.of_json
  ; status_message =
      Aws.Util.option_map (Aws.Json.lookup j "statusMessage") String.of_json
  ; capacity_manager_provided =
      Aws.Util.option_map (Aws.Json.lookup j "capacityManagerProvided") Boolean.of_json
  ; earliest_datapoint_timestamp =
      Aws.Util.option_map
        (Aws.Json.lookup j "earliestDatapointTimestamp")
        DateTime.of_json
  }
