open Aws.BaseTypes

type t =
  { capacity_reservation_fleet_id : String.t option
  ; state : CapacityReservationFleetState.t option
  ; total_target_capacity : Integer.t option
  ; total_fulfilled_capacity : Double.t option
  ; instance_match_criteria : FleetInstanceMatchCriteria.t option
  ; allocation_strategy : String.t option
  ; create_time : DateTime.t option
  ; end_date : DateTime.t option
  ; tenancy : FleetCapacityReservationTenancy.t option
  ; fleet_capacity_reservations : FleetCapacityReservationSet.t
  ; tags : TagList.t
  }

let make
    ?capacity_reservation_fleet_id
    ?state
    ?total_target_capacity
    ?total_fulfilled_capacity
    ?instance_match_criteria
    ?allocation_strategy
    ?create_time
    ?end_date
    ?tenancy
    ?(fleet_capacity_reservations = [])
    ?(tags = [])
    () =
  { capacity_reservation_fleet_id
  ; state
  ; total_target_capacity
  ; total_fulfilled_capacity
  ; instance_match_criteria
  ; allocation_strategy
  ; create_time
  ; end_date
  ; tenancy
  ; fleet_capacity_reservations
  ; tags
  }

let parse xml =
  Some
    { capacity_reservation_fleet_id =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationFleetId" xml)
          String.parse
    ; state =
        Aws.Util.option_bind
          (Aws.Xml.member "state" xml)
          CapacityReservationFleetState.parse
    ; total_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "totalTargetCapacity" xml) Integer.parse
    ; total_fulfilled_capacity =
        Aws.Util.option_bind (Aws.Xml.member "totalFulfilledCapacity" xml) Double.parse
    ; instance_match_criteria =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceMatchCriteria" xml)
          FleetInstanceMatchCriteria.parse
    ; allocation_strategy =
        Aws.Util.option_bind (Aws.Xml.member "allocationStrategy" xml) String.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; end_date = Aws.Util.option_bind (Aws.Xml.member "endDate" xml) DateTime.parse
    ; tenancy =
        Aws.Util.option_bind
          (Aws.Xml.member "tenancy" xml)
          FleetCapacityReservationTenancy.parse
    ; fleet_capacity_reservations =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "fleetCapacityReservationSet" xml)
             FleetCapacityReservationSet.parse)
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Some
           (Aws.Query.Pair
              ( "FleetCapacityReservationSet"
              , FleetCapacityReservationSet.to_query v.fleet_capacity_reservations ))
       ; Aws.Util.option_map v.tenancy (fun f ->
             Aws.Query.Pair ("Tenancy", FleetCapacityReservationTenancy.to_query f))
       ; Aws.Util.option_map v.end_date (fun f ->
             Aws.Query.Pair ("EndDate", DateTime.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             Aws.Query.Pair ("AllocationStrategy", String.to_query f))
       ; Aws.Util.option_map v.instance_match_criteria (fun f ->
             Aws.Query.Pair
               ("InstanceMatchCriteria", FleetInstanceMatchCriteria.to_query f))
       ; Aws.Util.option_map v.total_fulfilled_capacity (fun f ->
             Aws.Query.Pair ("TotalFulfilledCapacity", Double.to_query f))
       ; Aws.Util.option_map v.total_target_capacity (fun f ->
             Aws.Query.Pair ("TotalTargetCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", CapacityReservationFleetState.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_fleet_id (fun f ->
             Aws.Query.Pair ("CapacityReservationFleetId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("tagSet", TagList.to_json v.tags)
       ; Some
           ( "fleetCapacityReservationSet"
           , FleetCapacityReservationSet.to_json v.fleet_capacity_reservations )
       ; Aws.Util.option_map v.tenancy (fun f ->
             "tenancy", FleetCapacityReservationTenancy.to_json f)
       ; Aws.Util.option_map v.end_date (fun f -> "endDate", DateTime.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             "allocationStrategy", String.to_json f)
       ; Aws.Util.option_map v.instance_match_criteria (fun f ->
             "instanceMatchCriteria", FleetInstanceMatchCriteria.to_json f)
       ; Aws.Util.option_map v.total_fulfilled_capacity (fun f ->
             "totalFulfilledCapacity", Double.to_json f)
       ; Aws.Util.option_map v.total_target_capacity (fun f ->
             "totalTargetCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.state (fun f ->
             "state", CapacityReservationFleetState.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_fleet_id (fun f ->
             "capacityReservationFleetId", String.to_json f)
       ])

let of_json j =
  { capacity_reservation_fleet_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationFleetId") String.of_json
  ; state =
      Aws.Util.option_map
        (Aws.Json.lookup j "state")
        CapacityReservationFleetState.of_json
  ; total_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "totalTargetCapacity") Integer.of_json
  ; total_fulfilled_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "totalFulfilledCapacity") Double.of_json
  ; instance_match_criteria =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceMatchCriteria")
        FleetInstanceMatchCriteria.of_json
  ; allocation_strategy =
      Aws.Util.option_map (Aws.Json.lookup j "allocationStrategy") String.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; end_date = Aws.Util.option_map (Aws.Json.lookup j "endDate") DateTime.of_json
  ; tenancy =
      Aws.Util.option_map
        (Aws.Json.lookup j "tenancy")
        FleetCapacityReservationTenancy.of_json
  ; fleet_capacity_reservations =
      FleetCapacityReservationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "fleetCapacityReservationSet"))
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  }
