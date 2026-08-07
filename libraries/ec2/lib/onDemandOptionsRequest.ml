open Aws.BaseTypes

type t =
  { allocation_strategy : FleetOnDemandAllocationStrategy.t option
  ; capacity_reservation_options : CapacityReservationOptionsRequest.t option
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
          (Aws.Xml.member "AllocationStrategy" xml)
          FleetOnDemandAllocationStrategy.parse
    ; capacity_reservation_options =
        Aws.Util.option_bind
          (Aws.Xml.member "CapacityReservationOptions" xml)
          CapacityReservationOptionsRequest.parse
    ; single_instance_type =
        Aws.Util.option_bind (Aws.Xml.member "SingleInstanceType" xml) Boolean.parse
    ; single_availability_zone =
        Aws.Util.option_bind (Aws.Xml.member "SingleAvailabilityZone" xml) Boolean.parse
    ; min_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "MinTargetCapacity" xml) Integer.parse
    ; max_total_price =
        Aws.Util.option_bind (Aws.Xml.member "MaxTotalPrice" xml) String.parse
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
               ("CapacityReservationOptions", CapacityReservationOptionsRequest.to_query f))
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             Aws.Query.Pair
               ("AllocationStrategy", FleetOnDemandAllocationStrategy.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.max_total_price (fun f ->
             "MaxTotalPrice", String.to_json f)
       ; Aws.Util.option_map v.min_target_capacity (fun f ->
             "MinTargetCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.single_availability_zone (fun f ->
             "SingleAvailabilityZone", Boolean.to_json f)
       ; Aws.Util.option_map v.single_instance_type (fun f ->
             "SingleInstanceType", Boolean.to_json f)
       ; Aws.Util.option_map v.capacity_reservation_options (fun f ->
             "CapacityReservationOptions", CapacityReservationOptionsRequest.to_json f)
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             "AllocationStrategy", FleetOnDemandAllocationStrategy.to_json f)
       ])

let of_json j =
  { allocation_strategy =
      Aws.Util.option_map
        (Aws.Json.lookup j "AllocationStrategy")
        FleetOnDemandAllocationStrategy.of_json
  ; capacity_reservation_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "CapacityReservationOptions")
        CapacityReservationOptionsRequest.of_json
  ; single_instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "SingleInstanceType") Boolean.of_json
  ; single_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "SingleAvailabilityZone") Boolean.of_json
  ; min_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "MinTargetCapacity") Integer.of_json
  ; max_total_price =
      Aws.Util.option_map (Aws.Json.lookup j "MaxTotalPrice") String.of_json
  }
