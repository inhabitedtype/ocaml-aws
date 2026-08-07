open Aws.BaseTypes

type t =
  { allocation_strategy : SpotAllocationStrategy.t option
  ; maintenance_strategies : FleetSpotMaintenanceStrategies.t option
  ; instance_interruption_behavior : SpotInstanceInterruptionBehavior.t option
  ; instance_pools_to_use_count : Integer.t option
  ; single_instance_type : Boolean.t option
  ; single_availability_zone : Boolean.t option
  ; min_target_capacity : Integer.t option
  ; max_total_price : String.t option
  }

let make
    ?allocation_strategy
    ?maintenance_strategies
    ?instance_interruption_behavior
    ?instance_pools_to_use_count
    ?single_instance_type
    ?single_availability_zone
    ?min_target_capacity
    ?max_total_price
    () =
  { allocation_strategy
  ; maintenance_strategies
  ; instance_interruption_behavior
  ; instance_pools_to_use_count
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
          SpotAllocationStrategy.parse
    ; maintenance_strategies =
        Aws.Util.option_bind
          (Aws.Xml.member "maintenanceStrategies" xml)
          FleetSpotMaintenanceStrategies.parse
    ; instance_interruption_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceInterruptionBehavior" xml)
          SpotInstanceInterruptionBehavior.parse
    ; instance_pools_to_use_count =
        Aws.Util.option_bind (Aws.Xml.member "instancePoolsToUseCount" xml) Integer.parse
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
       ; Aws.Util.option_map v.instance_pools_to_use_count (fun f ->
             Aws.Query.Pair ("InstancePoolsToUseCount", Integer.to_query f))
       ; Aws.Util.option_map v.instance_interruption_behavior (fun f ->
             Aws.Query.Pair
               ( "InstanceInterruptionBehavior"
               , SpotInstanceInterruptionBehavior.to_query f ))
       ; Aws.Util.option_map v.maintenance_strategies (fun f ->
             Aws.Query.Pair
               ("MaintenanceStrategies", FleetSpotMaintenanceStrategies.to_query f))
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             Aws.Query.Pair ("AllocationStrategy", SpotAllocationStrategy.to_query f))
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
       ; Aws.Util.option_map v.instance_pools_to_use_count (fun f ->
             "instancePoolsToUseCount", Integer.to_json f)
       ; Aws.Util.option_map v.instance_interruption_behavior (fun f ->
             "instanceInterruptionBehavior", SpotInstanceInterruptionBehavior.to_json f)
       ; Aws.Util.option_map v.maintenance_strategies (fun f ->
             "maintenanceStrategies", FleetSpotMaintenanceStrategies.to_json f)
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             "allocationStrategy", SpotAllocationStrategy.to_json f)
       ])

let of_json j =
  { allocation_strategy =
      Aws.Util.option_map
        (Aws.Json.lookup j "allocationStrategy")
        SpotAllocationStrategy.of_json
  ; maintenance_strategies =
      Aws.Util.option_map
        (Aws.Json.lookup j "maintenanceStrategies")
        FleetSpotMaintenanceStrategies.of_json
  ; instance_interruption_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceInterruptionBehavior")
        SpotInstanceInterruptionBehavior.of_json
  ; instance_pools_to_use_count =
      Aws.Util.option_map (Aws.Json.lookup j "instancePoolsToUseCount") Integer.of_json
  ; single_instance_type =
      Aws.Util.option_map (Aws.Json.lookup j "singleInstanceType") Boolean.of_json
  ; single_availability_zone =
      Aws.Util.option_map (Aws.Json.lookup j "singleAvailabilityZone") Boolean.of_json
  ; min_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "minTargetCapacity") Integer.of_json
  ; max_total_price =
      Aws.Util.option_map (Aws.Json.lookup j "maxTotalPrice") String.of_json
  }
