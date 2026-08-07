open Aws.BaseTypes

type t =
  { activity_status : FleetActivityStatus.t option
  ; create_time : DateTime.t option
  ; fleet_id : String.t option
  ; fleet_state : FleetStateCode.t option
  ; client_token : String.t option
  ; excess_capacity_termination_policy : FleetExcessCapacityTerminationPolicy.t option
  ; fulfilled_capacity : Double.t option
  ; fulfilled_on_demand_capacity : Double.t option
  ; launch_template_configs : FleetLaunchTemplateConfigList.t
  ; target_capacity_specification : TargetCapacitySpecification.t option
  ; terminate_instances_with_expiration : Boolean.t option
  ; type_ : FleetType.t option
  ; valid_from : DateTime.t option
  ; valid_until : DateTime.t option
  ; replace_unhealthy_instances : Boolean.t option
  ; spot_options : SpotOptions.t option
  ; on_demand_options : OnDemandOptions.t option
  ; reserved_capacity_options : ReservedCapacityOptions.t option
  ; tags : TagList.t
  ; errors : DescribeFleetsErrorSet.t
  ; instances : DescribeFleetsInstancesSet.t
  ; context : String.t option
  }

let make
    ?activity_status
    ?create_time
    ?fleet_id
    ?fleet_state
    ?client_token
    ?excess_capacity_termination_policy
    ?fulfilled_capacity
    ?fulfilled_on_demand_capacity
    ?(launch_template_configs = [])
    ?target_capacity_specification
    ?terminate_instances_with_expiration
    ?type_
    ?valid_from
    ?valid_until
    ?replace_unhealthy_instances
    ?spot_options
    ?on_demand_options
    ?reserved_capacity_options
    ?(tags = [])
    ?(errors = [])
    ?(instances = [])
    ?context
    () =
  { activity_status
  ; create_time
  ; fleet_id
  ; fleet_state
  ; client_token
  ; excess_capacity_termination_policy
  ; fulfilled_capacity
  ; fulfilled_on_demand_capacity
  ; launch_template_configs
  ; target_capacity_specification
  ; terminate_instances_with_expiration
  ; type_
  ; valid_from
  ; valid_until
  ; replace_unhealthy_instances
  ; spot_options
  ; on_demand_options
  ; reserved_capacity_options
  ; tags
  ; errors
  ; instances
  ; context
  }

let parse xml =
  Some
    { activity_status =
        Aws.Util.option_bind
          (Aws.Xml.member "activityStatus" xml)
          FleetActivityStatus.parse
    ; create_time = Aws.Util.option_bind (Aws.Xml.member "createTime" xml) DateTime.parse
    ; fleet_id = Aws.Util.option_bind (Aws.Xml.member "fleetId" xml) String.parse
    ; fleet_state =
        Aws.Util.option_bind (Aws.Xml.member "fleetState" xml) FleetStateCode.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "clientToken" xml) String.parse
    ; excess_capacity_termination_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "excessCapacityTerminationPolicy" xml)
          FleetExcessCapacityTerminationPolicy.parse
    ; fulfilled_capacity =
        Aws.Util.option_bind (Aws.Xml.member "fulfilledCapacity" xml) Double.parse
    ; fulfilled_on_demand_capacity =
        Aws.Util.option_bind (Aws.Xml.member "fulfilledOnDemandCapacity" xml) Double.parse
    ; launch_template_configs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "launchTemplateConfigs" xml)
             FleetLaunchTemplateConfigList.parse)
    ; target_capacity_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "targetCapacitySpecification" xml)
          TargetCapacitySpecification.parse
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
    ; spot_options =
        Aws.Util.option_bind (Aws.Xml.member "spotOptions" xml) SpotOptions.parse
    ; on_demand_options =
        Aws.Util.option_bind (Aws.Xml.member "onDemandOptions" xml) OnDemandOptions.parse
    ; reserved_capacity_options =
        Aws.Util.option_bind
          (Aws.Xml.member "reservedCapacityOptions" xml)
          ReservedCapacityOptions.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; errors =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "errorSet" xml)
             DescribeFleetsErrorSet.parse)
    ; instances =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "fleetInstanceSet" xml)
             DescribeFleetsInstancesSet.parse)
    ; context = Aws.Util.option_bind (Aws.Xml.member "context" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.context (fun f ->
             Aws.Query.Pair ("Context", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("FleetInstanceSet", DescribeFleetsInstancesSet.to_query v.instances))
       ; Some (Aws.Query.Pair ("ErrorSet", DescribeFleetsErrorSet.to_query v.errors))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.reserved_capacity_options (fun f ->
             Aws.Query.Pair ("ReservedCapacityOptions", ReservedCapacityOptions.to_query f))
       ; Aws.Util.option_map v.on_demand_options (fun f ->
             Aws.Query.Pair ("OnDemandOptions", OnDemandOptions.to_query f))
       ; Aws.Util.option_map v.spot_options (fun f ->
             Aws.Query.Pair ("SpotOptions", SpotOptions.to_query f))
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
       ; Aws.Util.option_map v.target_capacity_specification (fun f ->
             Aws.Query.Pair
               ("TargetCapacitySpecification", TargetCapacitySpecification.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LaunchTemplateConfigs"
              , FleetLaunchTemplateConfigList.to_query v.launch_template_configs ))
       ; Aws.Util.option_map v.fulfilled_on_demand_capacity (fun f ->
             Aws.Query.Pair ("FulfilledOnDemandCapacity", Double.to_query f))
       ; Aws.Util.option_map v.fulfilled_capacity (fun f ->
             Aws.Query.Pair ("FulfilledCapacity", Double.to_query f))
       ; Aws.Util.option_map v.excess_capacity_termination_policy (fun f ->
             Aws.Query.Pair
               ( "ExcessCapacityTerminationPolicy"
               , FleetExcessCapacityTerminationPolicy.to_query f ))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.fleet_state (fun f ->
             Aws.Query.Pair ("FleetState", FleetStateCode.to_query f))
       ; Aws.Util.option_map v.fleet_id (fun f ->
             Aws.Query.Pair ("FleetId", String.to_query f))
       ; Aws.Util.option_map v.create_time (fun f ->
             Aws.Query.Pair ("CreateTime", DateTime.to_query f))
       ; Aws.Util.option_map v.activity_status (fun f ->
             Aws.Query.Pair ("ActivityStatus", FleetActivityStatus.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.context (fun f -> "context", String.to_json f)
       ; Some ("fleetInstanceSet", DescribeFleetsInstancesSet.to_json v.instances)
       ; Some ("errorSet", DescribeFleetsErrorSet.to_json v.errors)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.reserved_capacity_options (fun f ->
             "reservedCapacityOptions", ReservedCapacityOptions.to_json f)
       ; Aws.Util.option_map v.on_demand_options (fun f ->
             "onDemandOptions", OnDemandOptions.to_json f)
       ; Aws.Util.option_map v.spot_options (fun f ->
             "spotOptions", SpotOptions.to_json f)
       ; Aws.Util.option_map v.replace_unhealthy_instances (fun f ->
             "replaceUnhealthyInstances", Boolean.to_json f)
       ; Aws.Util.option_map v.valid_until (fun f -> "validUntil", DateTime.to_json f)
       ; Aws.Util.option_map v.valid_from (fun f -> "validFrom", DateTime.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "type", FleetType.to_json f)
       ; Aws.Util.option_map v.terminate_instances_with_expiration (fun f ->
             "terminateInstancesWithExpiration", Boolean.to_json f)
       ; Aws.Util.option_map v.target_capacity_specification (fun f ->
             "targetCapacitySpecification", TargetCapacitySpecification.to_json f)
       ; Some
           ( "launchTemplateConfigs"
           , FleetLaunchTemplateConfigList.to_json v.launch_template_configs )
       ; Aws.Util.option_map v.fulfilled_on_demand_capacity (fun f ->
             "fulfilledOnDemandCapacity", Double.to_json f)
       ; Aws.Util.option_map v.fulfilled_capacity (fun f ->
             "fulfilledCapacity", Double.to_json f)
       ; Aws.Util.option_map v.excess_capacity_termination_policy (fun f ->
             ( "excessCapacityTerminationPolicy"
             , FleetExcessCapacityTerminationPolicy.to_json f ))
       ; Aws.Util.option_map v.client_token (fun f -> "clientToken", String.to_json f)
       ; Aws.Util.option_map v.fleet_state (fun f ->
             "fleetState", FleetStateCode.to_json f)
       ; Aws.Util.option_map v.fleet_id (fun f -> "fleetId", String.to_json f)
       ; Aws.Util.option_map v.create_time (fun f -> "createTime", DateTime.to_json f)
       ; Aws.Util.option_map v.activity_status (fun f ->
             "activityStatus", FleetActivityStatus.to_json f)
       ])

let of_json j =
  { activity_status =
      Aws.Util.option_map (Aws.Json.lookup j "activityStatus") FleetActivityStatus.of_json
  ; create_time = Aws.Util.option_map (Aws.Json.lookup j "createTime") DateTime.of_json
  ; fleet_id = Aws.Util.option_map (Aws.Json.lookup j "fleetId") String.of_json
  ; fleet_state =
      Aws.Util.option_map (Aws.Json.lookup j "fleetState") FleetStateCode.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "clientToken") String.of_json
  ; excess_capacity_termination_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "excessCapacityTerminationPolicy")
        FleetExcessCapacityTerminationPolicy.of_json
  ; fulfilled_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "fulfilledCapacity") Double.of_json
  ; fulfilled_on_demand_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "fulfilledOnDemandCapacity") Double.of_json
  ; launch_template_configs =
      FleetLaunchTemplateConfigList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "launchTemplateConfigs"))
  ; target_capacity_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "targetCapacitySpecification")
        TargetCapacitySpecification.of_json
  ; terminate_instances_with_expiration =
      Aws.Util.option_map
        (Aws.Json.lookup j "terminateInstancesWithExpiration")
        Boolean.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "type") FleetType.of_json
  ; valid_from = Aws.Util.option_map (Aws.Json.lookup j "validFrom") DateTime.of_json
  ; valid_until = Aws.Util.option_map (Aws.Json.lookup j "validUntil") DateTime.of_json
  ; replace_unhealthy_instances =
      Aws.Util.option_map (Aws.Json.lookup j "replaceUnhealthyInstances") Boolean.of_json
  ; spot_options =
      Aws.Util.option_map (Aws.Json.lookup j "spotOptions") SpotOptions.of_json
  ; on_demand_options =
      Aws.Util.option_map (Aws.Json.lookup j "onDemandOptions") OnDemandOptions.of_json
  ; reserved_capacity_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "reservedCapacityOptions")
        ReservedCapacityOptions.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; errors =
      DescribeFleetsErrorSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "errorSet"))
  ; instances =
      DescribeFleetsInstancesSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "fleetInstanceSet"))
  ; context = Aws.Util.option_map (Aws.Json.lookup j "context") String.of_json
  }
