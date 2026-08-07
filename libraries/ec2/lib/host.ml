open Aws.BaseTypes

type t =
  { auto_placement : AutoPlacement.t option
  ; availability_zone : String.t option
  ; available_capacity : AvailableCapacity.t option
  ; client_token : String.t option
  ; host_id : String.t option
  ; host_properties : HostProperties.t option
  ; host_reservation_id : String.t option
  ; instances : HostInstanceList.t
  ; state : AllocationState.t option
  ; allocation_time : DateTime.t option
  ; release_time : DateTime.t option
  ; tags : TagList.t
  ; host_recovery : HostRecovery.t option
  ; allows_multiple_instance_types : AllowsMultipleInstanceTypes.t option
  ; owner_id : String.t option
  ; availability_zone_id : String.t option
  ; member_of_service_linked_resource_group : Boolean.t option
  ; outpost_arn : String.t option
  ; host_maintenance : HostMaintenance.t option
  ; asset_id : String.t option
  }

let make
    ?auto_placement
    ?availability_zone
    ?available_capacity
    ?client_token
    ?host_id
    ?host_properties
    ?host_reservation_id
    ?(instances = [])
    ?state
    ?allocation_time
    ?release_time
    ?(tags = [])
    ?host_recovery
    ?allows_multiple_instance_types
    ?owner_id
    ?availability_zone_id
    ?member_of_service_linked_resource_group
    ?outpost_arn
    ?host_maintenance
    ?asset_id
    () =
  { auto_placement
  ; availability_zone
  ; available_capacity
  ; client_token
  ; host_id
  ; host_properties
  ; host_reservation_id
  ; instances
  ; state
  ; allocation_time
  ; release_time
  ; tags
  ; host_recovery
  ; allows_multiple_instance_types
  ; owner_id
  ; availability_zone_id
  ; member_of_service_linked_resource_group
  ; outpost_arn
  ; host_maintenance
  ; asset_id
  }

let parse xml =
  Some
    { auto_placement =
        Aws.Util.option_bind (Aws.Xml.member "autoPlacement" xml) AutoPlacement.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; available_capacity =
        Aws.Util.option_bind
          (Aws.Xml.member "availableCapacity" xml)
          AvailableCapacity.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; host_id = Aws.Util.option_bind (Aws.Xml.member "hostId" xml) String.parse
    ; host_properties =
        Aws.Util.option_bind (Aws.Xml.member "hostProperties" xml) HostProperties.parse
    ; host_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "hostReservationId" xml) String.parse
    ; instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "instances" xml) HostInstanceList.parse)
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) AllocationState.parse
    ; allocation_time =
        Aws.Util.option_bind (Aws.Xml.member "allocationTime" xml) DateTime.parse
    ; release_time =
        Aws.Util.option_bind (Aws.Xml.member "releaseTime" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; host_recovery =
        Aws.Util.option_bind (Aws.Xml.member "hostRecovery" xml) HostRecovery.parse
    ; allows_multiple_instance_types =
        Aws.Util.option_bind
          (Aws.Xml.member "allowsMultipleInstanceTypes" xml)
          AllowsMultipleInstanceTypes.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; member_of_service_linked_resource_group =
        Aws.Util.option_bind
          (Aws.Xml.member "memberOfServiceLinkedResourceGroup" xml)
          Boolean.parse
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; host_maintenance =
        Aws.Util.option_bind (Aws.Xml.member "hostMaintenance" xml) HostMaintenance.parse
    ; asset_id = Aws.Util.option_bind (Aws.Xml.member "assetId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.asset_id (fun f ->
             Aws.Query.Pair ("AssetId", String.to_query f))
       ; Aws.Util.option_map v.host_maintenance (fun f ->
             Aws.Query.Pair ("HostMaintenance", HostMaintenance.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Aws.Util.option_map v.member_of_service_linked_resource_group (fun f ->
             Aws.Query.Pair ("MemberOfServiceLinkedResourceGroup", Boolean.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.allows_multiple_instance_types (fun f ->
             Aws.Query.Pair
               ("AllowsMultipleInstanceTypes", AllowsMultipleInstanceTypes.to_query f))
       ; Aws.Util.option_map v.host_recovery (fun f ->
             Aws.Query.Pair ("HostRecovery", HostRecovery.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.release_time (fun f ->
             Aws.Query.Pair ("ReleaseTime", DateTime.to_query f))
       ; Aws.Util.option_map v.allocation_time (fun f ->
             Aws.Query.Pair ("AllocationTime", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", AllocationState.to_query f))
       ; Some (Aws.Query.Pair ("Instances", HostInstanceList.to_query v.instances))
       ; Aws.Util.option_map v.host_reservation_id (fun f ->
             Aws.Query.Pair ("HostReservationId", String.to_query f))
       ; Aws.Util.option_map v.host_properties (fun f ->
             Aws.Query.Pair ("HostProperties", HostProperties.to_query f))
       ; Aws.Util.option_map v.host_id (fun f ->
             Aws.Query.Pair ("HostId", String.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.available_capacity (fun f ->
             Aws.Query.Pair ("AvailableCapacity", AvailableCapacity.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.auto_placement (fun f ->
             Aws.Query.Pair ("AutoPlacement", AutoPlacement.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.asset_id (fun f -> "assetId", String.to_json f)
       ; Aws.Util.option_map v.host_maintenance (fun f ->
             "hostMaintenance", HostMaintenance.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Aws.Util.option_map v.member_of_service_linked_resource_group (fun f ->
             "memberOfServiceLinkedResourceGroup", Boolean.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.allows_multiple_instance_types (fun f ->
             "allowsMultipleInstanceTypes", AllowsMultipleInstanceTypes.to_json f)
       ; Aws.Util.option_map v.host_recovery (fun f ->
             "hostRecovery", HostRecovery.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.release_time (fun f -> "releaseTime", DateTime.to_json f)
       ; Aws.Util.option_map v.allocation_time (fun f ->
             "allocationTime", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f -> "state", AllocationState.to_json f)
       ; Some ("instances", HostInstanceList.to_json v.instances)
       ; Aws.Util.option_map v.host_reservation_id (fun f ->
             "hostReservationId", String.to_json f)
       ; Aws.Util.option_map v.host_properties (fun f ->
             "hostProperties", HostProperties.to_json f)
       ; Aws.Util.option_map v.host_id (fun f -> "hostId", String.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.available_capacity (fun f ->
             "availableCapacity", AvailableCapacity.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.auto_placement (fun f ->
             "autoPlacement", AutoPlacement.to_json f)
       ])

let of_json j =
  { auto_placement =
      Aws.Util.option_map (Aws.Json.lookup j "autoPlacement") AutoPlacement.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; available_capacity =
      Aws.Util.option_map
        (Aws.Json.lookup j "availableCapacity")
        AvailableCapacity.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; host_id = Aws.Util.option_map (Aws.Json.lookup j "hostId") String.of_json
  ; host_properties =
      Aws.Util.option_map (Aws.Json.lookup j "hostProperties") HostProperties.of_json
  ; host_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "hostReservationId") String.of_json
  ; instances =
      HostInstanceList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "instances"))
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") AllocationState.of_json
  ; allocation_time =
      Aws.Util.option_map (Aws.Json.lookup j "allocationTime") DateTime.of_json
  ; release_time = Aws.Util.option_map (Aws.Json.lookup j "releaseTime") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; host_recovery =
      Aws.Util.option_map (Aws.Json.lookup j "hostRecovery") HostRecovery.of_json
  ; allows_multiple_instance_types =
      Aws.Util.option_map
        (Aws.Json.lookup j "allowsMultipleInstanceTypes")
        AllowsMultipleInstanceTypes.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; member_of_service_linked_resource_group =
      Aws.Util.option_map
        (Aws.Json.lookup j "memberOfServiceLinkedResourceGroup")
        Boolean.of_json
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; host_maintenance =
      Aws.Util.option_map (Aws.Json.lookup j "hostMaintenance") HostMaintenance.of_json
  ; asset_id = Aws.Util.option_map (Aws.Json.lookup j "assetId") String.of_json
  }
