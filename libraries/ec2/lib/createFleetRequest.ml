open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; client_token : String.t option
  ; spot_options : SpotOptionsRequest.t option
  ; on_demand_options : OnDemandOptionsRequest.t option
  ; reserved_capacity_options : ReservedCapacityOptionsRequest.t option
  ; excess_capacity_termination_policy : FleetExcessCapacityTerminationPolicy.t option
  ; launch_template_configs : FleetLaunchTemplateConfigListRequest.t
  ; target_capacity_specification : TargetCapacitySpecificationRequest.t
  ; terminate_instances_with_expiration : Boolean.t option
  ; type_ : FleetType.t option
  ; valid_from : DateTime.t option
  ; valid_until : DateTime.t option
  ; replace_unhealthy_instances : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  ; context : String.t option
  }

let make
    ?dry_run
    ?client_token
    ?spot_options
    ?on_demand_options
    ?reserved_capacity_options
    ?excess_capacity_termination_policy
    ~launch_template_configs
    ~target_capacity_specification
    ?terminate_instances_with_expiration
    ?type_
    ?valid_from
    ?valid_until
    ?replace_unhealthy_instances
    ?(tag_specifications = [])
    ?context
    () =
  { dry_run
  ; client_token
  ; spot_options
  ; on_demand_options
  ; reserved_capacity_options
  ; excess_capacity_termination_policy
  ; launch_template_configs
  ; target_capacity_specification
  ; terminate_instances_with_expiration
  ; type_
  ; valid_from
  ; valid_until
  ; replace_unhealthy_instances
  ; tag_specifications
  ; context
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; spot_options =
        Aws.Util.option_bind (Aws.Xml.member "SpotOptions" xml) SpotOptionsRequest.parse
    ; on_demand_options =
        Aws.Util.option_bind
          (Aws.Xml.member "OnDemandOptions" xml)
          OnDemandOptionsRequest.parse
    ; reserved_capacity_options =
        Aws.Util.option_bind
          (Aws.Xml.member "ReservedCapacityOptions" xml)
          ReservedCapacityOptionsRequest.parse
    ; excess_capacity_termination_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "ExcessCapacityTerminationPolicy" xml)
          FleetExcessCapacityTerminationPolicy.parse
    ; launch_template_configs =
        Aws.Xml.required
          "LaunchTemplateConfigs"
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchTemplateConfigs" xml)
             FleetLaunchTemplateConfigListRequest.parse)
    ; target_capacity_specification =
        Aws.Xml.required
          "TargetCapacitySpecification"
          (Aws.Util.option_bind
             (Aws.Xml.member "TargetCapacitySpecification" xml)
             TargetCapacitySpecificationRequest.parse)
    ; terminate_instances_with_expiration =
        Aws.Util.option_bind
          (Aws.Xml.member "TerminateInstancesWithExpiration" xml)
          Boolean.parse
    ; type_ = Aws.Util.option_bind (Aws.Xml.member "Type" xml) FleetType.parse
    ; valid_from = Aws.Util.option_bind (Aws.Xml.member "ValidFrom" xml) DateTime.parse
    ; valid_until = Aws.Util.option_bind (Aws.Xml.member "ValidUntil" xml) DateTime.parse
    ; replace_unhealthy_instances =
        Aws.Util.option_bind
          (Aws.Xml.member "ReplaceUnhealthyInstances" xml)
          Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    ; context = Aws.Util.option_bind (Aws.Xml.member "Context" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.context (fun f ->
             Aws.Query.Pair ("Context", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
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
       ; Some
           (Aws.Query.Pair
              ( "TargetCapacitySpecification"
              , TargetCapacitySpecificationRequest.to_query
                  v.target_capacity_specification ))
       ; Some
           (Aws.Query.Pair
              ( "LaunchTemplateConfigs"
              , FleetLaunchTemplateConfigListRequest.to_query v.launch_template_configs ))
       ; Aws.Util.option_map v.excess_capacity_termination_policy (fun f ->
             Aws.Query.Pair
               ( "ExcessCapacityTerminationPolicy"
               , FleetExcessCapacityTerminationPolicy.to_query f ))
       ; Aws.Util.option_map v.reserved_capacity_options (fun f ->
             Aws.Query.Pair
               ("ReservedCapacityOptions", ReservedCapacityOptionsRequest.to_query f))
       ; Aws.Util.option_map v.on_demand_options (fun f ->
             Aws.Query.Pair ("OnDemandOptions", OnDemandOptionsRequest.to_query f))
       ; Aws.Util.option_map v.spot_options (fun f ->
             Aws.Query.Pair ("SpotOptions", SpotOptionsRequest.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.context (fun f -> "Context", String.to_json f)
       ; Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.replace_unhealthy_instances (fun f ->
             "ReplaceUnhealthyInstances", Boolean.to_json f)
       ; Aws.Util.option_map v.valid_until (fun f -> "ValidUntil", DateTime.to_json f)
       ; Aws.Util.option_map v.valid_from (fun f -> "ValidFrom", DateTime.to_json f)
       ; Aws.Util.option_map v.type_ (fun f -> "Type", FleetType.to_json f)
       ; Aws.Util.option_map v.terminate_instances_with_expiration (fun f ->
             "TerminateInstancesWithExpiration", Boolean.to_json f)
       ; Some
           ( "TargetCapacitySpecification"
           , TargetCapacitySpecificationRequest.to_json v.target_capacity_specification )
       ; Some
           ( "LaunchTemplateConfigs"
           , FleetLaunchTemplateConfigListRequest.to_json v.launch_template_configs )
       ; Aws.Util.option_map v.excess_capacity_termination_policy (fun f ->
             ( "ExcessCapacityTerminationPolicy"
             , FleetExcessCapacityTerminationPolicy.to_json f ))
       ; Aws.Util.option_map v.reserved_capacity_options (fun f ->
             "ReservedCapacityOptions", ReservedCapacityOptionsRequest.to_json f)
       ; Aws.Util.option_map v.on_demand_options (fun f ->
             "OnDemandOptions", OnDemandOptionsRequest.to_json f)
       ; Aws.Util.option_map v.spot_options (fun f ->
             "SpotOptions", SpotOptionsRequest.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; spot_options =
      Aws.Util.option_map (Aws.Json.lookup j "SpotOptions") SpotOptionsRequest.of_json
  ; on_demand_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "OnDemandOptions")
        OnDemandOptionsRequest.of_json
  ; reserved_capacity_options =
      Aws.Util.option_map
        (Aws.Json.lookup j "ReservedCapacityOptions")
        ReservedCapacityOptionsRequest.of_json
  ; excess_capacity_termination_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "ExcessCapacityTerminationPolicy")
        FleetExcessCapacityTerminationPolicy.of_json
  ; launch_template_configs =
      FleetLaunchTemplateConfigListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchTemplateConfigs"))
  ; target_capacity_specification =
      TargetCapacitySpecificationRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TargetCapacitySpecification"))
  ; terminate_instances_with_expiration =
      Aws.Util.option_map
        (Aws.Json.lookup j "TerminateInstancesWithExpiration")
        Boolean.of_json
  ; type_ = Aws.Util.option_map (Aws.Json.lookup j "Type") FleetType.of_json
  ; valid_from = Aws.Util.option_map (Aws.Json.lookup j "ValidFrom") DateTime.of_json
  ; valid_until = Aws.Util.option_map (Aws.Json.lookup j "ValidUntil") DateTime.of_json
  ; replace_unhealthy_instances =
      Aws.Util.option_map (Aws.Json.lookup j "ReplaceUnhealthyInstances") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  ; context = Aws.Util.option_map (Aws.Json.lookup j "Context") String.of_json
  }
