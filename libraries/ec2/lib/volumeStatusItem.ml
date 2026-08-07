open Aws.BaseTypes

type t =
  { actions : VolumeStatusActionsList.t
  ; availability_zone : String.t option
  ; outpost_arn : String.t option
  ; events : VolumeStatusEventsList.t
  ; volume_id : String.t option
  ; volume_status : VolumeStatusInfo.t option
  ; attachment_statuses : VolumeStatusAttachmentStatusList.t
  ; initialization_status_details : InitializationStatusDetails.t option
  ; availability_zone_id : String.t option
  ; operator : OperatorResponse.t option
  }

let make
    ?(actions = [])
    ?availability_zone
    ?outpost_arn
    ?(events = [])
    ?volume_id
    ?volume_status
    ?(attachment_statuses = [])
    ?initialization_status_details
    ?availability_zone_id
    ?operator
    () =
  { actions
  ; availability_zone
  ; outpost_arn
  ; events
  ; volume_id
  ; volume_status
  ; attachment_statuses
  ; initialization_status_details
  ; availability_zone_id
  ; operator
  }

let parse xml =
  Some
    { actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "actionsSet" xml)
             VolumeStatusActionsList.parse)
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; events =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "eventsSet" xml)
             VolumeStatusEventsList.parse)
    ; volume_id = Aws.Util.option_bind (Aws.Xml.member "volumeId" xml) String.parse
    ; volume_status =
        Aws.Util.option_bind (Aws.Xml.member "volumeStatus" xml) VolumeStatusInfo.parse
    ; attachment_statuses =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "attachmentStatuses" xml)
             VolumeStatusAttachmentStatusList.parse)
    ; initialization_status_details =
        Aws.Util.option_bind
          (Aws.Xml.member "initializationStatusDetails" xml)
          InitializationStatusDetails.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; operator =
        Aws.Util.option_bind (Aws.Xml.member "operator" xml) OperatorResponse.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f ->
             Aws.Query.Pair ("Operator", OperatorResponse.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.initialization_status_details (fun f ->
             Aws.Query.Pair
               ("InitializationStatusDetails", InitializationStatusDetails.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "AttachmentStatuses"
              , VolumeStatusAttachmentStatusList.to_query v.attachment_statuses ))
       ; Aws.Util.option_map v.volume_status (fun f ->
             Aws.Query.Pair ("VolumeStatus", VolumeStatusInfo.to_query f))
       ; Aws.Util.option_map v.volume_id (fun f ->
             Aws.Query.Pair ("VolumeId", String.to_query f))
       ; Some (Aws.Query.Pair ("EventsSet", VolumeStatusEventsList.to_query v.events))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Some (Aws.Query.Pair ("ActionsSet", VolumeStatusActionsList.to_query v.actions))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.operator (fun f -> "operator", OperatorResponse.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.initialization_status_details (fun f ->
             "initializationStatusDetails", InitializationStatusDetails.to_json f)
       ; Some
           ( "attachmentStatuses"
           , VolumeStatusAttachmentStatusList.to_json v.attachment_statuses )
       ; Aws.Util.option_map v.volume_status (fun f ->
             "volumeStatus", VolumeStatusInfo.to_json f)
       ; Aws.Util.option_map v.volume_id (fun f -> "volumeId", String.to_json f)
       ; Some ("eventsSet", VolumeStatusEventsList.to_json v.events)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Some ("actionsSet", VolumeStatusActionsList.to_json v.actions)
       ])

let of_json j =
  { actions =
      VolumeStatusActionsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "actionsSet"))
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; events =
      VolumeStatusEventsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "eventsSet"))
  ; volume_id = Aws.Util.option_map (Aws.Json.lookup j "volumeId") String.of_json
  ; volume_status =
      Aws.Util.option_map (Aws.Json.lookup j "volumeStatus") VolumeStatusInfo.of_json
  ; attachment_statuses =
      VolumeStatusAttachmentStatusList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "attachmentStatuses"))
  ; initialization_status_details =
      Aws.Util.option_map
        (Aws.Json.lookup j "initializationStatusDetails")
        InitializationStatusDetails.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; operator = Aws.Util.option_map (Aws.Json.lookup j "operator") OperatorResponse.of_json
  }
