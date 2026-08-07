open Aws.BaseTypes

type t =
  { snapshot_id : String.t option
  ; availability_zone : String.t option
  ; availability_zone_id : String.t option
  ; state : FastSnapshotRestoreStateCode.t option
  ; state_transition_reason : String.t option
  ; owner_id : String.t option
  ; owner_alias : String.t option
  ; enabling_time : DateTime.t option
  ; optimizing_time : DateTime.t option
  ; enabled_time : DateTime.t option
  ; disabling_time : DateTime.t option
  ; disabled_time : DateTime.t option
  }

let make
    ?snapshot_id
    ?availability_zone
    ?availability_zone_id
    ?state
    ?state_transition_reason
    ?owner_id
    ?owner_alias
    ?enabling_time
    ?optimizing_time
    ?enabled_time
    ?disabling_time
    ?disabled_time
    () =
  { snapshot_id
  ; availability_zone
  ; availability_zone_id
  ; state
  ; state_transition_reason
  ; owner_id
  ; owner_alias
  ; enabling_time
  ; optimizing_time
  ; enabled_time
  ; disabling_time
  ; disabled_time
  }

let parse xml =
  Some
    { snapshot_id = Aws.Util.option_bind (Aws.Xml.member "snapshotId" xml) String.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          FastSnapshotRestoreStateCode.parse
    ; state_transition_reason =
        Aws.Util.option_bind (Aws.Xml.member "stateTransitionReason" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; owner_alias = Aws.Util.option_bind (Aws.Xml.member "ownerAlias" xml) String.parse
    ; enabling_time =
        Aws.Util.option_bind (Aws.Xml.member "enablingTime" xml) DateTime.parse
    ; optimizing_time =
        Aws.Util.option_bind (Aws.Xml.member "optimizingTime" xml) DateTime.parse
    ; enabled_time =
        Aws.Util.option_bind (Aws.Xml.member "enabledTime" xml) DateTime.parse
    ; disabling_time =
        Aws.Util.option_bind (Aws.Xml.member "disablingTime" xml) DateTime.parse
    ; disabled_time =
        Aws.Util.option_bind (Aws.Xml.member "disabledTime" xml) DateTime.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.disabled_time (fun f ->
             Aws.Query.Pair ("DisabledTime", DateTime.to_query f))
       ; Aws.Util.option_map v.disabling_time (fun f ->
             Aws.Query.Pair ("DisablingTime", DateTime.to_query f))
       ; Aws.Util.option_map v.enabled_time (fun f ->
             Aws.Query.Pair ("EnabledTime", DateTime.to_query f))
       ; Aws.Util.option_map v.optimizing_time (fun f ->
             Aws.Query.Pair ("OptimizingTime", DateTime.to_query f))
       ; Aws.Util.option_map v.enabling_time (fun f ->
             Aws.Query.Pair ("EnablingTime", DateTime.to_query f))
       ; Aws.Util.option_map v.owner_alias (fun f ->
             Aws.Query.Pair ("OwnerAlias", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.state_transition_reason (fun f ->
             Aws.Query.Pair ("StateTransitionReason", String.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", FastSnapshotRestoreStateCode.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.snapshot_id (fun f ->
             Aws.Query.Pair ("SnapshotId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.disabled_time (fun f -> "disabledTime", DateTime.to_json f)
       ; Aws.Util.option_map v.disabling_time (fun f ->
             "disablingTime", DateTime.to_json f)
       ; Aws.Util.option_map v.enabled_time (fun f -> "enabledTime", DateTime.to_json f)
       ; Aws.Util.option_map v.optimizing_time (fun f ->
             "optimizingTime", DateTime.to_json f)
       ; Aws.Util.option_map v.enabling_time (fun f -> "enablingTime", DateTime.to_json f)
       ; Aws.Util.option_map v.owner_alias (fun f -> "ownerAlias", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.state_transition_reason (fun f ->
             "stateTransitionReason", String.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", FastSnapshotRestoreStateCode.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.snapshot_id (fun f -> "snapshotId", String.to_json f)
       ])

let of_json j =
  { snapshot_id = Aws.Util.option_map (Aws.Json.lookup j "snapshotId") String.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") FastSnapshotRestoreStateCode.of_json
  ; state_transition_reason =
      Aws.Util.option_map (Aws.Json.lookup j "stateTransitionReason") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; owner_alias = Aws.Util.option_map (Aws.Json.lookup j "ownerAlias") String.of_json
  ; enabling_time =
      Aws.Util.option_map (Aws.Json.lookup j "enablingTime") DateTime.of_json
  ; optimizing_time =
      Aws.Util.option_map (Aws.Json.lookup j "optimizingTime") DateTime.of_json
  ; enabled_time = Aws.Util.option_map (Aws.Json.lookup j "enabledTime") DateTime.of_json
  ; disabling_time =
      Aws.Util.option_map (Aws.Json.lookup j "disablingTime") DateTime.of_json
  ; disabled_time =
      Aws.Util.option_map (Aws.Json.lookup j "disabledTime") DateTime.of_json
  }
