open Aws.BaseTypes

type t =
  { availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; outpost_arn : String.t option
  ; operator : OperatorResponse.t option
  ; events : InstanceStatusEventList.t
  ; instance_id : String.t option
  ; instance_state : InstanceState.t option
  ; instance_status : InstanceStatusSummary.t option
  ; system_status : InstanceStatusSummary.t option
  ; attached_ebs_status : EbsStatusSummary.t option
  }

let make
    ?availability_zone
    ?availability_zone_id
    ?outpost_arn
    ?operator
    ?(events = [])
    ?instance_id
    ?instance_state
    ?instance_status
    ?system_status
    ?attached_ebs_status
    () =
  { availability_zone
  ; availability_zone_id
  ; outpost_arn
  ; operator
  ; events
  ; instance_id
  ; instance_state
  ; instance_status
  ; system_status
  ; attached_ebs_status
  }

let parse xml =
  Some
    { availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    ; events =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "eventsSet" xml)
             InstanceStatusEventList.parse)
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "instanceId" xml) String.parse
    ; instance_state =
        Aws.Util.option_bind (Aws.Xml.member "instanceState" xml) InstanceState.parse
    ; instance_status =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceStatus" xml)
          InstanceStatusSummary.parse
    ; system_status =
        Aws.Util.option_bind
          (Aws.Xml.member "systemStatus" xml)
          InstanceStatusSummary.parse
    ; attached_ebs_status =
        Aws.Util.option_bind
          (Aws.Xml.member "attachedEbsStatus" xml)
          EbsStatusSummary.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attached_ebs_status (fun f ->
             Aws.Query.Pair ("AttachedEbsStatus", EbsStatusSummary.to_query f))
       ; Aws.Util.option_map v.system_status (fun f ->
             Aws.Query.Pair ("SystemStatus", InstanceStatusSummary.to_query f))
       ; Aws.Util.option_map v.instance_status (fun f ->
             Aws.Query.Pair ("InstanceStatus", InstanceStatusSummary.to_query f))
       ; Aws.Util.option_map v.instance_state (fun f ->
             Aws.Query.Pair ("InstanceState", InstanceState.to_query f))
       ; Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Some (Aws.Query.Pair ("EventsSet", InstanceStatusEventList.to_query v.events))
       ; Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.attached_ebs_status (fun f ->
             "attachedEbsStatus", EbsStatusSummary.to_json f)
       ; Aws.Util.option_map v.system_status (fun f ->
             "systemStatus", InstanceStatusSummary.to_json f)
       ; Aws.Util.option_map v.instance_status (fun f ->
             "instanceStatus", InstanceStatusSummary.to_json f)
       ; Aws.Util.option_map v.instance_state (fun f ->
             "instanceState", InstanceState.to_json f)
       ; Aws.Util.option_map v.instance_id (fun f -> "instanceId", String.to_json f)
       ; Some ("eventsSet", InstanceStatusEventList.to_json v.events)
       ; Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ])

let of_json j =
  { availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  ; events =
      InstanceStatusEventList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "eventsSet"))
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "instanceId") String.of_json
  ; instance_state =
      Aws.Util.option_map (Aws.Json.lookup j "instanceState") InstanceState.of_json
  ; instance_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceStatus")
        InstanceStatusSummary.of_json
  ; system_status =
      Aws.Util.option_map (Aws.Json.lookup j "systemStatus") InstanceStatusSummary.of_json
  ; attached_ebs_status =
      Aws.Util.option_map (Aws.Json.lookup j "attachedEbsStatus") EbsStatusSummary.of_json
  }
