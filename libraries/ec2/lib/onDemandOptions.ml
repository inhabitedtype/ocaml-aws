open Aws.BaseTypes

type t =
  { allocation_strategy : FleetOnDemandAllocationStrategy.t option
  ; capacity_reservation_options : CapacityReservationOptions.t option
  ; single_instance_type : Boolean.t option
  ; single_availability_zone : Boolean.t option
  ; min_target_capacity : Integer.t option
  ; max_total_price : String.t option
  }

let make
    ?allocation_strategy
    ?capacity_reservation_options
    ?single_instance_type
    ?single_availability_zone
    ?min_target_capacity
    ?max_total_price
    () =
  { allocation_strategy
  ; capacity_reservation_options
  ; single_instance_type
  ; single_availability_zone
  ; min_target_capacity
  ; max_total_price
  }

let parse xml =
  Some
    { allocation_strategy =
        Aws.Util.option_bind
          (Aws.Xml.member "allocationStrategy" xml)
          FleetOnDemandAllocationStrategy.parse
    ; capacity_reservation_options =
        Aws.Util.option_bind
          (Aws.Xml.member "capacityReservationOptions" xml)
          CapacityReservationOptions.parse
    ; single_instance_type =
        Aws.Util.option_bind (Aws.Xml.member "singleInstanceType" xml) Boolean.parse
    ; single_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "singleAvailabilityZone" xml) Boolean.parse
    ; min_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "minTargetCapacity" xml) Integer.parse
    ; max_total_price =
        Aws.Util.option_bind (Aws.Xml.member "maxTotalPrice" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_total_price (fun f ->
             Aws.Query.Pair ("MaxTotalPrice", String.to_query f))
       ; Aws.Util.option_map v.min_target_capacity (fun f ->
             Aws.Query.Pair ("MinTargetCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.single_availability_zone (fun f ->
             Aws.Query.Pair ("SingleAvailabilityZone", Boolean.to_query f))
       ; Aws.Util.option_map v.single_instance_type (fun f ->
             Aws.Query.Pair ("SingleInstanceType", Boolean.to_query f))
       ; Aws.Util.option_map v.capacity_reservation_options (fun f ->
             Aws.Query.Pair
               ("CapacityReservationOptions", CapacityReservationOptions.to_query f))
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             Aws.Query.Pair
               ("AllocationStrategy", FleetOnDemandAllocationStrategy.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_total_price (fun f ->
             "maxTotalPrice", String.to_json f)
       ; Aws.Util.option_map v.min_target_capacity (fun f ->
             "minTargetCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.single_availability_zone (fun f ->
             "singleAvailabilityZone", Boolean.to_json f)
       ; Aws.Util.option_map v.single_instance_type (fun f ->
             "singleInstanceType", Boolean.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_options (fun f ->
             "capacityReservationOptions", CapacityReservationOptions.to_json f)
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             "allocationStrategy", FleetOnDemandAllocationStrategy.to_json f)
       ])

let of_json j =
  { allocation_strategy =
      Aws.Util.option_map
        (Aws.Json.lookup j "allocationStrategy")
        FleetOnDemandAllocationStrategy.of_json
  ; capacity_reservation_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "capacityReservationOptions")
        CapacityReservationOptions.of_json
  ; single_instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "singleInstanceType") Boolean.of_json
  ; single_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "singleAvailabilityZone") Boolean.of_json
  ; min_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "minTargetCapacity") Integer.of_json
  ; max_total_price =
      Aws.Util.option_map (Aws.Json.lookup j "maxTotalPrice") String.of_json
  }
