open Aws.BaseTypes

type t =
  { next_token : String.t option
  ; capacity_reservation_id : String.t option
  ; instance_type : String.t option
  ; total_instance_count : Integer.t option
  ; available_instance_count : Integer.t option
  ; state : CapacityReservationState.t option
  ; instance_usages : InstanceUsageSet.t
  ; interruptible : Boolean.t option
  ; interruptible_capacity_allocation : InterruptibleCapacityAllocation.t option
  ; interruption_info : InterruptionInfo.t option
  }

let make
    ?next_token
    ?capacity_reservation_id
    ?instance_type
    ?total_instance_count
    ?available_instance_count
    ?state
    ?(instance_usages = [])
    ?interruptible
    ?interruptible_capacity_allocation
    ?interruption_info
    () =
  { next_token
  ; capacity_reservation_id
  ; instance_type
  ; total_instance_count
  ; available_instance_count
  ; state
  ; instance_usages
  ; interruptible
  ; interruptible_capacity_allocation
  ; interruption_info
  }

let parse xml =
  Some
    { next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    ; capacity_reservation_id =
        Aws.Util.option_bind (Aws.Xml.member "capacityReservationId" xml) String.parse
    ; instance_type =
        Aws.Util.option_bind (Aws.Xml.member "instanceType" xml) String.parse
    ; total_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "totalInstanceCount" xml) Integer.parse
    ; available_instance_count =
        Aws.Util.option_bind (Aws.Xml.member "availableInstanceCount" xml) Integer.parse
    ; state =
        Aws.Util.option_bind (Aws.Xml.member "state" xml) CapacityReservationState.parse
    ; instance_usages =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "instanceUsageSet" xml)
             InstanceUsageSet.parse)
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
       ; Some
           (Aws.Query.Pair
              ("InstanceUsageSet", InstanceUsageSet.to_query v.instance_usages))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", CapacityReservationState.to_query f))
       ; Aws.Util.option_map v.available_instance_count (fun f ->
             Aws.Query.Pair ("AvailableInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.total_instance_count (fun f ->
             Aws.Query.Pair ("TotalInstanceCount", Integer.to_query f))
       ; Aws.Util.option_map v.instance_type (fun f ->
             Aws.Query.Pair ("InstanceType", String.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             Aws.Query.Pair ("CapacityReservationId", String.to_query f))
       ; Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.interruption_info (fun f ->
             "interruptionInfo", InterruptionInfo.to_json f)
       ; Aws.Util.option_map v.interruptible_capacity_allocation (fun f ->
             "interruptibleCapacityAllocation", InterruptibleCapacityAllocation.to_json f)
       ; Aws.Util.option_map v.interruptible (fun f -> "interruptible", Boolean.to_json f)
       ; Some ("instanceUsageSet", InstanceUsageSet.to_json v.instance_usages)
       ; Aws.Util.option_map v.state (fun f ->
             "state", CapacityReservationState.to_json f)
       ; Aws.Util.option_map v.available_instance_count (fun f ->
             "availableInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.total_instance_count (fun f ->
             "totalInstanceCount", Integer.to_json f)
       ; Aws.Util.option_map v.instance_type (fun f -> "instanceType", String.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_id (fun f ->
             "capacityReservationId", String.to_json f)
       ; Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ])

let of_json j =
  { next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  ; capacity_reservation_id =
      Aws.Util.option_map (Aws.Json.lookup j "capacityReservationId") String.of_json
  ; instance_type = Aws.Util.option_map (Aws.Json.lookup j "instanceType") String.of_json
  ; total_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "totalInstanceCount") Integer.of_json
  ; available_instance_count =
      Aws.Util.option_map (Aws.Json.lookup j "availableInstanceCount") Integer.of_json
  ; state =
      Aws.Util.option_map (Aws.Json.lookup j "state") CapacityReservationState.of_json
  ; instance_usages =
      InstanceUsageSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "instanceUsageSet"))
  ; interruptible =
      Aws.Util.option_map (Aws.Json.lookup j "interruptible") Boolean.of_json
  ; interruptible_capacity_allocation =
      Aws.Util.option_map
        (Aws.Json.lookup j "interruptibleCapacityAllocation")
        InterruptibleCapacityAllocation.of_json
  ; interruption_info =
      Aws.Util.option_map (Aws.Json.lookup j "interruptionInfo") InterruptionInfo.of_json
  }
