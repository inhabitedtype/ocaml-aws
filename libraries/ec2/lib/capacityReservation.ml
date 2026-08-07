open Aws.BaseTypes

type t =
  { capacity_reservation_id : String.t option
  ; owner_id : String.t option
  ; capacity_reservation_arn : String.t option
  ; availability_zone_id : String.t option
  ; instance_type : String.t option
  ; instance_platform : CapacityReservationInstancePlatform.t option
  ; availability_zone : String.t option
  ; tenancy : CapacityReservationTenancy.t option
  ; total_instance_count : Integer.t option
  ; available_instance_count : Integer.t option
  ; ebs_optimized : Boolean.t option
  ; ephemeral_storage : Boolean.t option
  ; state : CapacityReservationState.t option
  ; start_date : DateTime.t option
  ; end_date : DateTime.t option
  ; end_date_type : EndDateType.t option
  ; instance_match_criteria : InstanceMatchCriteria.t option
  ; create_date : DateTime.t option
  ; tags : TagList.t
  ; outpost_arn : String.t option
  ; capacity_reservation_fleet_id : String.t option
  ; placement_group_arn : String.t option
  ; capacity_allocations : CapacityAllocations.t
  ; reservation_type : CapacityReservationType.t option
  ; unused_reservation_billing_owner_id : String.t option
  ; commitment_info : CapacityReservationCommitmentInfo.t option
  ; delivery_preference : CapacityReservationDeliveryPreference.t option
  ; capacity_block_id : String.t option
  ; interruptible : Boolean.t option
  ; interruptible_capacity_allocation : InterruptibleCapacityAllocation.t option
  ; interruption_info : InterruptionInfo.t option
  }

let make
    ?capacity_reservation_id
    ?owner_id
    ?capacity_reservation_arn
    ?availability_zone_id
    ?instance_type
    ?instance_platform
    ?availability_zone
    ?tenancy
    ?total_instance_count
    ?available_instance_count
    ?ebs_optimized
    ?ephemeral_storage
    ?state
    ?start_date
    ?end_date
    ?end_date_type
    ?instance_match_criteria
    ?create_date
    ?(tags = [])
    ?outpost_arn
    ?capacity_reservation_fleet_id
    ?placement_group_arn
    ?(capacity_allocations = [])
    ?reservation_type
    ?unused_reservation_billing_owner_id
    ?commitment_info
    ?delivery_preference
    ?capacity_block_id
    ?interruptible
    ?interruptible_capacity_allocation
    ?interruption_info
    () =
  { capacity_reservation_id
  ; owner_id
  ; capacity_reservation_arn
  ; availability_zone_id
  ; instance_type
  ; instance_platform
  ; availability_zone
  ; tenancy
  ; total_instance_count
  ; available_instance_count
  ; ebs_optimized
  ; ephemeral_storage
  ; state
  ; start_date
  ; end_date
  ; end_date_type
  ; instance_match_criteria
  ; create_date
  ; tags
  ; outpost_arn
  ; capacity_reservation_fleet_id
  ; placement_group_arn
  ; capacity_allocations
  ; reservation_type
  ; unused_reservation_billing_owner_id
  ; commitment_info
  ; delivery_preference
  ; capacity_block_id
  ; interruptible
  ; interruptible_capacity_allocation
  ; interruption_info
  }

let parse xml =
  Some
    { capacity_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityReservationId" xml) String.parse
    ; owner_id = Aws.Util.option_bind (Aws.Xml.member "ownerId" xml) String.parse
    ; capacity_reservation_arn =
        Aws.Util.option_bind (Aws.Xml.member "capacityReservationArn" xml) String.parse
    ; availability_zone_id =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZoneId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; instance_platform =
        Aws.Util.option_bind
          (Aws.Xml.member "instancePlatform" xml)
          CapacityReservationInstancePlatform.parse
    ; availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "availabilityZone" xml) String.parse
    ; tenancy =
        Aws.Util.option_bind
          (Aws.Xml.member "tenancy" xml)
          CapacityReservationTenancy.parse
    ; total_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "totalInstanceCount" xml) Integer.parse
    ; available_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "availableInstanceCount" xml) Integer.parse
    ; ebs_optimized =
        Aws.Util.option_bind (Aws.Xml.member "ebsOptimized" xml) Boolean.parse
    ; ephemeral_storage =
        Aws.Util.option_bind (Aws.Xml.member "ephemeralStorage" xml) Boolean.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) CapacityReservationState.parse
    ; start_date = Aws.Util.option_bind (Aws.Xml.member "startDate" xml) DateTime.parse
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "endDate" xml) DateTime.parse
    ; end_date_type =
        Aws.Util.option_bind (Aws.Xml.member "endDateType" xml) EndDateType.parse
    ; instance_match_criteria =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceMatchCriteria" xml)
          InstanceMatchCriteria.parse
    ; create_date = Aws.Util.option_bind (Aws.Xml.member "createDate" xml) DateTime.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; outpost_arn = Aws.Util.option_bind (Aws.Xml.member "outpostArn" xml) String.parse
    ; capacity_reservation_fleet_id =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationFleetId" xml)
          String.parse
    ; placement_group_arn =
        Aws.Util.option_bind (Aws.Xml.member "placementGroupArn" xml) String.parse
    ; capacity_allocations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "capacityAllocationSet" xml)
             CapacityAllocations.parse)
    ; reservation_type =
        Aws.Util.option_bind
          (Aws.Xml.member "reservationType" xml)
          CapacityReservationType.parse
    ; unused_reservation_billing_owner_id =
        Aws.Util.option_bind
          (Aws.Xml.member "unusedReservationBillingOwnerId" xml)
          String.parse
    ; commitment_info =
        Aws.Util.option_bind
          (Aws.Xml.member "commitmentInfo" xml)
          CapacityReservationCommitmentInfo.parse
    ; delivery_preference =
        Aws.Util.option_bind
          (Aws.Xml.member "deliveryPreference" xml)
          CapacityReservationDeliveryPreference.parse
    ; capacity_block_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityBlockId" xml) String.parse
    ; interruptible =
        Aws.Util.option_bind (Aws.Xml.member "interruptible" xml) Boolean.parse
    ; interruptible_capacity_allocation =
        Aws.Util.option_bind
          (Aws.Xml.member "interruptibleCapacityAllocation" xml)
          InterruptibleCapacityAllocation.parse
    ; interruption_info =
        Aws.Util.option_bind
          (Aws.Xml.member "interruptionInfo" xml)
          InterruptionInfo.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.interruption_info (fun f ->
             Aws.Query.Pair ("InterruptionInfo", InterruptionInfo.to_query f))
       ; Aws.Util.option_map v.interruptible_capacity_allocation (fun f ->
             Aws.Query.Pair
               ( "InterruptibleCapacityAllocation"
               , InterruptibleCapacityAllocation.to_query f ))
       ; Aws.Util.option_map v.interruptible (fun f ->
             Aws.Query.Pair ("Interruptible", Boolean.to_query f))
       ; Aws.Util.option_map v.capacity_block_id (fun f ->
             Aws.Query.Pair ("CapacityBlockId", String.to_query f))
       ; Aws.Util.option_map v.delivery_preference (fun f ->
             Aws.Query.Pair
               ("DeliveryPreference", CapacityReservationDeliveryPreference.to_query f))
       ; Aws.Util.option_map v.commitment_info (fun f ->
             Aws.Query.Pair
               ("CommitmentInfo", CapacityReservationCommitmentInfo.to_query f))
       ; Aws.Util.option_map v.unused_reservation_billing_owner_id (fun f ->
             Aws.Query.Pair ("UnusedReservationBillingOwnerId", String.to_query f))
       ; Aws.Util.option_map v.reservation_type (fun f ->
             Aws.Query.Pair ("ReservationType", CapacityReservationType.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "CapacityAllocationSet"
              , CapacityAllocations.to_query v.capacity_allocations ))
       ; Aws.Util.option_map v.placement_group_arn (fun f ->
             Aws.Query.Pair ("PlacementGroupArn", String.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_fleet_id (fun f ->
             Aws.Query.Pair ("CapacityReservationFleetId", String.to_query f))
       ; Aws.Util.option_map v.outpost_arn (fun f ->
             Aws.Query.Pair ("OutpostArn", String.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.create_date (fun f ->
             Aws.Query.Pair ("CreateDate", DateTime.to_query f))
       ; Aws.Util.option_map v.instance_match_criteria (fun f ->
             Aws.Query.Pair ("InstanceMatchCriteria", InstanceMatchCriteria.to_query f))
       ; Aws.Util.option_map v.end_date_type (fun f ->
             Aws.Query.Pair ("EndDateType", EndDateType.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.start_date (fun f ->
             Aws.Query.Pair ("StartDate", DateTime.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", CapacityReservationState.to_query f))
       ; Aws.Util.option_map v.ephemeral_storage (fun f ->
             Aws.Query.Pair ("EphemeralStorage", Boolean.to_query f))
       ; Aws.Util.option_map v.ebs_optimized (fun f ->
             Aws.Query.Pair ("EbsOptimized", Boolean.to_query f))
       ; Aws.Util.option_map v.available_instance_count (fun f ->
             Aws.Query.Pair ("AvailableInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.total_instance_count (fun f ->
             Aws.Query.Pair ("TotalInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", CapacityReservationTenancy.to_query f))
       ; Aws.Util.option_map v.availability_zone (fun f ->
             Aws.Query.Pair ("AvailabilityZone", String.to_query f))
       ; Aws.Util.option_map v.instance_platform (fun f ->
             Aws.Query.Pair
               ("InstancePlatform", CapacityReservationInstancePlatform.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             Aws.Query.Pair ("AvailabilityZoneId", String.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_arn (fun f ->
             Aws.Query.Pair ("CapacityReservationArn", String.to_query f))
       ; Aws.Util.option_map v.owner_id (fun f ->
             Aws.Query.Pair ("OwnerId", String.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             Aws.Query.Pair ("CapacityReservationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.interruption_info (fun f ->
             "interruptionInfo", InterruptionInfo.to_json f)
       ; Aws.Util.option_map v.interruptible_capacity_allocation (fun f ->
             "interruptibleCapacityAllocation", InterruptibleCapacityAllocation.to_json f)
       ; Aws.Util.option_map v.interruptible (fun f -> "interruptible", Boolean.to_json f)
       ; Aws.Util.option_map v.capacity_block_id (fun f ->
             "capacityBlockId", String.to_json f)
       ; Aws.Util.option_map v.delivery_preference (fun f ->
             "deliveryPreference", CapacityReservationDeliveryPreference.to_json f)
       ; Aws.Util.option_map v.commitment_info (fun f ->
             "commitmentInfo", CapacityReservationCommitmentInfo.to_json f)
       ; Aws.Util.option_map v.unused_reservation_billing_owner_id (fun f ->
             "unusedReservationBillingOwnerId", String.to_json f)
       ; Aws.Util.option_map v.reservation_type (fun f ->
             "reservationType", CapacityReservationType.to_json f)
       ; Some ("capacityAllocationSet", CapacityAllocations.to_json v.capacity_allocations)
       ; Aws.Util.option_map v.placement_group_arn (fun f ->
             "placementGroupArn", String.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_fleet_id (fun f ->
             "capacityReservationFleetId", String.to_json f)
       ; Aws.Util.option_map v.outpost_arn (fun f -> "outpostArn", String.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.create_date (fun f -> "createDate", DateTime.to_json f)
       ; Aws.Util.option_map v.instance_match_criteria (fun f ->
             "instanceMatchCriteria", InstanceMatchCriteria.to_json f)
       ; Aws.Util.option_map v.end_date_type (fun f ->
             "endDateType", EndDateType.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "endDate", DateTime.to_json f)
       ; Aws.Util.option_map v.start_date (fun f -> "startDate", DateTime.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", CapacityReservationState.to_json f)
       ; Aws.Util.option_map v.ephemeral_storage (fun f ->
             "ephemeralStorage", Boolean.to_json f)
       ; Aws.Util.option_map v.ebs_optimized (fun f -> "ebsOptimized", Boolean.to_json f)
       ; Aws.Util.option_map v.available_instance_count (fun f ->
             "availableInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.total_instance_count (fun f ->
             "totalInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.tenancy (fun f ->
             "tenancy", CapacityReservationTenancy.to_json f)
       ; Aws.Util.option_map v.availability_zone (fun f ->
             "availabilityZone", String.to_json f)
       ; Aws.Util.option_map v.instance_platform (fun f ->
             "instancePlatform", CapacityReservationInstancePlatform.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.availability_zone_id (fun f ->
             "availabilityZoneId", String.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_arn (fun f ->
             "capacityReservationArn", String.to_json f)
       ; Aws.Util.option_map v.owner_id (fun f -> "ownerId", String.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             "capacityReservationId", String.to_json f)
       ])

let of_json j =
  { capacity_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationId") String.of_json
  ; owner_id = Aws.Util.option_map (Aws.Json.lookup j "ownerId") String.of_json
  ; capacity_reservation_arn =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationArn") String.of_json
  ; availability_zone_id =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZoneId") String.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; instance_platform =
      Aws.Util.option_map
        (Aws.Json.lookup j "instancePlatform")
        CapacityReservationInstancePlatform.of_json
  ; availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "availabilityZone") String.of_json
  ; tenancy =
      Aws.Util.option_map (Aws.Json.lookup j "tenancy") CapacityReservationTenancy.of_json
  ; total_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "totalInstanceCount") Integer.of_json
  ; available_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "availableInstanceCount") Integer.of_json
  ; ebs_optimized = Aws.Util.option_map (Aws.Json.lookup j "ebsOptimized") Boolean.of_json
  ; ephemeral_storage =
      Aws.Util.option_map (Aws.Json.lookup j "ephemeralStorage") Boolean.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") CapacityReservationState.of_json
  ; start_date = Aws.Util.option_map (Aws.Json.lookup j "startDate") DateTime.of_json
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "endDate") DateTime.of_json
  ; end_date_type =
      Aws.Util.option_map (Aws.Json.lookup j "endDateType") EndDateType.of_json
  ; instance_match_criteria =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceMatchCriteria")
        InstanceMatchCriteria.of_json
  ; create_date = Aws.Util.option_map (Aws.Json.lookup j "createDate") DateTime.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; outpost_arn = Aws.Util.option_map (Aws.Json.lookup j "outpostArn") String.of_json
  ; capacity_reservation_fleet_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationFleetId") String.of_json
  ; placement_group_arn =
      Aws.Util.option_map (Aws.Json.lookup j "placementGroupArn") String.of_json
  ; capacity_allocations =
      CapacityAllocations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "capacityAllocationSet"))
  ; reservation_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "reservationType")
        CapacityReservationType.of_json
  ; unused_reservation_billing_owner_id =
      Aws.Util.option_map
        (Aws.Json.lookup j "unusedReservationBillingOwnerId")
        String.of_json
  ; commitment_info =
      Aws.Util.option_map
        (Aws.Json.lookup j "commitmentInfo")
        CapacityReservationCommitmentInfo.of_json
  ; delivery_preference =
      Aws.Util.option_map
        (Aws.Json.lookup j "deliveryPreference")
        CapacityReservationDeliveryPreference.of_json
  ; capacity_block_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityBlockId") String.of_json
  ; interruptible =
      Aws.Util.option_map (Aws.Json.lookup j "interruptible") Boolean.of_json
  ; interruptible_capacity_allocation =
      Aws.Util.option_map
        (Aws.Json.lookup j "interruptibleCapacityAllocation")
        InterruptibleCapacityAllocation.of_json
  ; interruption_info =
      Aws.Util.option_map (Aws.Json.lookup j "interruptionInfo") InterruptionInfo.of_json
  }
