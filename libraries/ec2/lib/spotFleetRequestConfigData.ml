open Aws.BaseTypes

type t =
  { allocation_strategy : AllocationStrategy.t option
  ; on_demand_allocation_strategy : OnDemandAllocationStrategy.t option
  ; spot_maintenance_strategies : SpotMaintenanceStrategies.t option
  ; client_token : String.t option
  ; excess_capacity_termination_policy : ExcessCapacityTerminationPolicy.t option
  ; fulfilled_capacity : Double.t option
  ; on_demand_fulfilled_capacity : Double.t option
  ; iam_fleet_role : String.t
  ; launch_specifications : LaunchSpecsList.t
  ; launch_template_configs : LaunchTemplateConfigList.t
  ; spot_price : String.t option
  ; target_capacity : Integer.t
  ; on_demand_target_capacity : Integer.t option
  ; on_demand_max_total_price : String.t option
  ; spot_max_total_price : String.t option
  ; terminate_instances_with_expiration : Boolean.t option
  ; type_ : FleetType.t option
  ; valid_from : DateTime.t option
  ; valid_until : DateTime.t option
  ; replace_unhealthy_instances : Boolean.t option
  ; instance_interruption_behavior : InstanceInterruptionBehavior.t option
  ; load_balancers_config : LoadBalancersConfig.t option
  ; instance_pools_to_use_count : Integer.t option
  ; context : String.t option
  ; target_capacity_unit_type : TargetCapacityUnitType.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ?allocation_strategy
    ?on_demand_allocation_strategy
    ?spot_maintenance_strategies
    ?client_token
    ?excess_capacity_termination_policy
    ?fulfilled_capacity
    ?on_demand_fulfilled_capacity
    ~iam_fleet_role
    ?(launch_specifications = [])
    ?(launch_template_configs = [])
    ?spot_price
    ~target_capacity
    ?on_demand_target_capacity
    ?on_demand_max_total_price
    ?spot_max_total_price
    ?terminate_instances_with_expiration
    ?type_
    ?valid_from
    ?valid_until
    ?replace_unhealthy_instances
    ?instance_interruption_behavior
    ?load_balancers_config
    ?instance_pools_to_use_count
    ?context
    ?target_capacity_unit_type
    ?(tag_specifications = [])
    () =
  { allocation_strategy
  ; on_demand_allocation_strategy
  ; spot_maintenance_strategies
  ; client_token
  ; excess_capacity_termination_policy
  ; fulfilled_capacity
  ; on_demand_fulfilled_capacity
  ; iam_fleet_role
  ; launch_specifications
  ; launch_template_configs
  ; spot_price
  ; target_capacity
  ; on_demand_target_capacity
  ; on_demand_max_total_price
  ; spot_max_total_price
  ; terminate_instances_with_expiration
  ; type_
  ; valid_from
  ; valid_until
  ; replace_unhealthy_instances
  ; instance_interruption_behavior
  ; load_balancers_config
  ; instance_pools_to_use_count
  ; context
  ; target_capacity_unit_type
  ; tag_specifications
  }

let parse xml =
  Some
    { allocation_strategy =
        Aws.Util.option_bind
          (Aws.Xml.member "allocationStrategy" xml)
          AllocationStrategy.parse
    ; on_demand_allocation_strategy =
        Aws.Util.option_bind
          (Aws.Xml.member "onDemandAllocationStrategy" xml)
          OnDemandAllocationStrategy.parse
    ; spot_maintenance_strategies =
        Aws.Util.option_bind
          (Aws.Xml.member "spotMaintenanceStrategies" xml)
          SpotMaintenanceStrategies.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; excess_capacity_termination_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "excessCapacityTerminationPolicy" xml)
          ExcessCapacityTerminationPolicy.parse
    ; fulfilled_capacity =
        Aws.Util.option_bind (Aws.Xml.member "fulfilledCapacity" xml) Double.parse
    ; on_demand_fulfilled_capacity =
        Aws.Util.option_bind (Aws.Xml.member "onDemandFulfilledCapacity" xml) Double.parse
    ; iam_fleet_role =
        Aws.Xml.required
          "iamFleetRole"
          (Aws.Util.option_bind (Aws.Xml.member "iamFleetRole" xml) String.parse)
    ; launch_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "launchSpecifications" xml)
             LaunchSpecsList.parse)
    ; launch_template_configs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "launchTemplateConfigs" xml)
             LaunchTemplateConfigList.parse)
    ; spot_price = Aws.Util.option_bind (Aws.Xml.member "spotPrice" xml) String.parse
    ; target_capacity =
        Aws.Xml.required
          "targetCapacity"
          (Aws.Util.option_bind (Aws.Xml.member "targetCapacity" xml) Integer.parse)
    ; on_demand_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "onDemandTargetCapacity" xml) Integer.parse
    ; on_demand_max_total_price =
        Aws.Util.option_bind (Aws.Xml.member "onDemandMaxTotalPrice" xml) String.parse
    ; spot_max_total_price =
        Aws.Util.option_bind (Aws.Xml.member "spotMaxTotalPrice" xml) String.parse
    ; terminate_instances_with_expiration =
        Aws.Util.option_bind
          (Aws.Xml.member "terminateInstancesWithExpiration" xml)
          Boolean.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "type" xml) FleetType.parse
    ; valid_from = Aws.Util.option_bind (Aws.Xml.member "validFrom" xml) DateTime.parse
    ; valid_until = Aws.Util.option_bind (Aws.Xml.member "validUntil" xml) DateTime.parse
    ; replace_unhealthy_instances =
        Aws.Util.option_bind
          (Aws.Xml.member "replaceUnhealthyInstances" xml)
          Boolean.parse
    ; instance_interruption_behavior =
        Aws.Util.option_bind
          (Aws.Xml.member "instanceInterruptionBehavior" xml)
          InstanceInterruptionBehavior.parse
    ; load_balancers_config =
        Aws.Util.option_bind
          (Aws.Xml.member "loadBalancersConfig" xml)
          LoadBalancersConfig.parse
    ; instance_pools_to_use_count =
        Aws.Util.option_bind (Aws.Xml.member "instancePoolsToUseCount" xml) Integer.parse
    ; context = Aws.Util.option_bind (Aws.Xml.member "context" xml) String.parse
    ; target_capacity_unit_type =
        Aws.Util.option_bind
          (Aws.Xml.member "targetCapacityUnitType" xml)
          TargetCapacityUnitType.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.target_capacity_unit_type (fun f ->
             Aws.Query.Pair ("TargetCapacityUnitType", TargetCapacityUnitType.to_query f))
       ; Aws.Util.option_map v.context (fun f ->
             Aws.Query.Pair ("Context", String.to_query f))
       ; Aws.Util.option_map v.instance_pools_to_use_count (fun f ->
             Aws.Query.Pair ("InstancePoolsToUseCount", Integer.to_query f))
       ; Aws.Util.option_map v.load_balancers_config (fun f ->
             Aws.Query.Pair ("LoadBalancersConfig", LoadBalancersConfig.to_query f))
       ; Aws.Util.option_map v.instance_interruption_behavior (fun f ->
             Aws.Query.Pair
               ("InstanceInterruptionBehavior", InstanceInterruptionBehavior.to_query f))
       ; Aws.Util.option_map v.replace_unhealthy_instances (fun f ->
             Aws.Query.Pair ("ReplaceUnhealthyInstances", Boolean.to_query f))
       ; Aws.Util.option_map v.valid_until (fun f ->
             Aws.Query.Pair ("ValidUntil", DateTime.to_query f))
       ; Aws.Util.option_map v.valid_from (fun f ->
             Aws.Query.Pair ("ValidFrom", DateTime.to_query f))
       ; Aws.Util.option_map v.type_ (fun f ->
             Aws.Query.Pair ("Type", FleetType.to_query f))
       ; Aws.Util.option_map v.terminate_instances_with_expiration (fun f ->
             Aws.Query.Pair ("TerminateInstancesWithExpiration", Boolean.to_query f))
       ; Aws.Util.option_map v.spot_max_total_price (fun f ->
             Aws.Query.Pair ("SpotMaxTotalPrice", String.to_query f))
       ; Aws.Util.option_map v.on_demand_max_total_price (fun f ->
             Aws.Query.Pair ("OnDemandMaxTotalPrice", String.to_query f))
       ; Aws.Util.option_map v.on_demand_target_capacity (fun f ->
             Aws.Query.Pair ("OnDemandTargetCapacity", Integer.to_query f))
       ; Some (Aws.Query.Pair ("TargetCapacity", Integer.to_query v.target_capacity))
       ; Aws.Util.option_map v.spot_price (fun f ->
             Aws.Query.Pair ("SpotPrice", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LaunchTemplateConfigs"
              , LaunchTemplateConfigList.to_query v.launch_template_configs ))
       ; Some
           (Aws.Query.Pair
              ("LaunchSpecifications", LaunchSpecsList.to_query v.launch_specifications))
       ; Some (Aws.Query.Pair ("IamFleetRole", String.to_query v.iam_fleet_role))
       ; Aws.Util.option_map v.on_demand_fulfilled_capacity (fun f ->
             Aws.Query.Pair ("OnDemandFulfilledCapacity", Double.to_query f))
       ; Aws.Util.option_map v.fulfilled_capacity (fun f ->
             Aws.Query.Pair ("FulfilledCapacity", Double.to_query f))
       ; Aws.Util.option_map v.excess_capacity_termination_policy (fun f ->
             Aws.Query.Pair
               ( "ExcessCapacityTerminationPolicy"
               , ExcessCapacityTerminationPolicy.to_query f ))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.spot_maintenance_strategies (fun f ->
             Aws.Query.Pair
               ("SpotMaintenanceStrategies", SpotMaintenanceStrategies.to_query f))
       ; Aws.Util.option_map v.on_demand_allocation_strategy (fun f ->
             Aws.Query.Pair
               ("OnDemandAllocationStrategy", OnDemandAllocationStrategy.to_query f))
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             Aws.Query.Pair ("AllocationStrategy", AllocationStrategy.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.target_capacity_unit_type (fun f ->
             "targetCapacityUnitType", TargetCapacityUnitType.to_json f)
       ; Aws.Util.option_map v.context (fun f -> "context", String.to_json f)
       ; Aws.Util.option_map v.instance_pools_to_use_count (fun f ->
             "instancePoolsToUseCount", Integer.to_json f)
       ; Aws.Util.option_map v.load_balancers_config (fun f ->
             "loadBalancersConfig", LoadBalancersConfig.to_json f)
       ; Aws.Util.option_map v.instance_interruption_behavior (fun f ->
             "instanceInterruptionBehavior", InstanceInterruptionBehavior.to_json f)
       ; Aws.Util.option_map v.replace_unhealthy_instances (fun f ->
             "replaceUnhealthyInstances", Boolean.to_json f)
       ; Aws.Util.option_map v.valid_until (fun f -> "validUntil", DateTime.to_json f)
       ; Aws.Util.option_map v.valid_from (fun f -> "validFrom", DateTime.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", FleetType.to_json f)
       ; Aws.Util.option_map v.terminate_instances_with_expiration (fun f ->
             "terminateInstancesWithExpiration", Boolean.to_json f)
       ; Aws.Util.option_map v.spot_max_total_price (fun f ->
             "spotMaxTotalPrice", String.to_json f)
       ; Aws.Util.option_map v.on_demand_max_total_price (fun f ->
             "onDemandMaxTotalPrice", String.to_json f)
       ; Aws.Util.option_map v.on_demand_target_capacity (fun f ->
             "onDemandTargetCapacity", Integer.to_json f)
       ; Some ("targetCapacity", Integer.to_json v.target_capacity)
       ; Aws.Util.option_map v.spot_price (fun f -> "spotPrice", String.to_json f)
       ; Some
           ( "launchTemplateConfigs"
           , LaunchTemplateConfigList.to_json v.launch_template_configs )
       ; Some ("launchSpecifications", LaunchSpecsList.to_json v.launch_specifications)
       ; Some ("iamFleetRole", String.to_json v.iam_fleet_role)
       ; Aws.Util.option_map v.on_demand_fulfilled_capacity (fun f ->
             "onDemandFulfilledCapacity", Double.to_json f)
       ; Aws.Util.option_map v.fulfilled_capacity (fun f ->
             "fulfilledCapacity", Double.to_json f)
       ; Aws.Util.option_map v.excess_capacity_termination_policy (fun f ->
             "excessCapacityTerminationPolicy", ExcessCapacityTerminationPolicy.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.spot_maintenance_strategies (fun f ->
             "spotMaintenanceStrategies", SpotMaintenanceStrategies.to_json f)
       ; Aws.Util.option_map v.on_demand_allocation_strategy (fun f ->
             "onDemandAllocationStrategy", OnDemandAllocationStrategy.to_json f)
       ; Aws.Util.option_map v.allocation_strategy (fun f ->
             "allocationStrategy", AllocationStrategy.to_json f)
       ])

let of_json j =
  { allocation_strategy =
      Aws.Util.option_map
        (Aws.Json.lookup j "allocationStrategy")
        AllocationStrategy.of_json
  ; on_demand_allocation_strategy =
      Aws.Util.option_map
        (Aws.Json.lookup j "onDemandAllocationStrategy")
        OnDemandAllocationStrategy.of_json
  ; spot_maintenance_strategies =
      Aws.Util.option_map
        (Aws.Json.lookup j "spotMaintenanceStrategies")
        SpotMaintenanceStrategies.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; excess_capacity_termination_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "excessCapacityTerminationPolicy")
        ExcessCapacityTerminationPolicy.of_json
  ; fulfilled_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "fulfilledCapacity") Double.of_json
  ; on_demand_fulfilled_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "onDemandFulfilledCapacity") Double.of_json
  ; iam_fleet_role =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "iamFleetRole"))
  ; launch_specifications =
      LaunchSpecsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "launchSpecifications"))
  ; launch_template_configs =
      LaunchTemplateConfigList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "launchTemplateConfigs"))
  ; spot_price = Aws.Util.option_map (Aws.Json.lookup j "spotPrice") String.of_json
  ; target_capacity =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "targetCapacity"))
  ; on_demand_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "onDemandTargetCapacity") Integer.of_json
  ; on_demand_max_total_price =
      Aws.Util.option_map (Aws.Json.lookup j "onDemandMaxTotalPrice") String.of_json
  ; spot_max_total_price =
      Aws.Util.option_map (Aws.Json.lookup j "spotMaxTotalPrice") String.of_json
  ; terminate_instances_with_expiration =
      Aws.Util.option_map
        (Aws.Json.lookup j "terminateInstancesWithExpiration")
        Boolean.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") FleetType.of_json
  ; valid_from = Aws.Util.option_map (Aws.Json.lookup j "validFrom") DateTime.of_json
  ; valid_until = Aws.Util.option_map (Aws.Json.lookup j "validUntil") DateTime.of_json
  ; replace_unhealthy_instances =
      Aws.Util.option_map (Aws.Json.lookup j "replaceUnhealthyInstances") Boolean.of_json
  ; instance_interruption_behavior =
      Aws.Util.option_map
        (Aws.Json.lookup j "instanceInterruptionBehavior")
        InstanceInterruptionBehavior.of_json
  ; load_balancers_config =
      Aws.Util.option_map
        (Aws.Json.lookup j "loadBalancersConfig")
        LoadBalancersConfig.of_json
  ; instance_pools_to_use_count =
      Aws.Util.option_map (Aws.Json.lookup j "instancePoolsToUseCount") Integer.of_json
  ; context = Aws.Util.option_map (Aws.Json.lookup j "context") String.of_json
  ; target_capacity_unit_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "targetCapacityUnitType")
        TargetCapacityUnitType.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
