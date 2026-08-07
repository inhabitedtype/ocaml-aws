open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; excess_capacity_termination_policy : FleetExcessCapacityTerminationPolicy.t option
  ; launch_template_configs : FleetLaunchTemplateConfigListRequest.t
  ; fleet_id : String.t
  ; target_capacity_specification : TargetCapacitySpecificationRequest.t option
  ; context : String.t option
  }

let make
    ?dry_run
    ?excess_capacity_termination_policy
    ?(launch_template_configs = [])
    ~fleet_id
    ?target_capacity_specification
    ?context
    () =
  { dry_run
  ; excess_capacity_termination_policy
  ; launch_template_configs
  ; fleet_id
  ; target_capacity_specification
  ; context
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; excess_capacity_termination_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "ExcessCapacityTerminationPolicy" xml)
          FleetExcessCapacityTerminationPolicy.parse
    ; launch_template_configs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchTemplateConfig" xml)
             FleetLaunchTemplateConfigListRequest.parse)
    ; fleet_id =
        Aws.Xml.required
          "FleetId"
          (Aws.Util.option_bind (Aws.Xml.member "FleetId" xml) String.parse)
    ; target_capacity_specification =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetCapacitySpecification" xml)
          TargetCapacitySpecificationRequest.parse
    ; context = Aws.Util.option_bind (Aws.Xml.member "Context" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.context (fun f ->
             Aws.Query.Pair ("Context", String.to_query f))
       ; Aws.Util.option_map v.target_capacity_specification (fun f ->
             Aws.Query.Pair
               ( "TargetCapacitySpecification"
               , TargetCapacitySpecificationRequest.to_query f ))
       ; Some (Aws.Query.Pair ("FleetId", String.to_query v.fleet_id))
       ; Some
           (Aws.Query.Pair
              ( "LaunchTemplateConfig"
              , FleetLaunchTemplateConfigListRequest.to_query v.launch_template_configs ))
       ; Aws.Util.option_map v.excess_capacity_termination_policy (fun f ->
             Aws.Query.Pair
               ( "ExcessCapacityTerminationPolicy"
               , FleetExcessCapacityTerminationPolicy.to_query f ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.context (fun f -> "Context", String.to_json f)
       ; Aws.Util.option_map v.target_capacity_specification (fun f ->
             "TargetCapacitySpecification", TargetCapacitySpecificationRequest.to_json f)
       ; Some ("FleetId", String.to_json v.fleet_id)
       ; Some
           ( "LaunchTemplateConfig"
           , FleetLaunchTemplateConfigListRequest.to_json v.launch_template_configs )
       ; Aws.Util.option_map v.excess_capacity_termination_policy (fun f ->
             ( "ExcessCapacityTerminationPolicy"
             , FleetExcessCapacityTerminationPolicy.to_json f ))
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; excess_capacity_termination_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "ExcessCapacityTerminationPolicy")
        FleetExcessCapacityTerminationPolicy.of_json
  ; launch_template_configs =
      FleetLaunchTemplateConfigListRequest.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchTemplateConfig"))
  ; fleet_id = String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "FleetId"))
  ; target_capacity_specification =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetCapacitySpecification")
        TargetCapacitySpecificationRequest.of_json
  ; context = Aws.Util.option_map (Aws.Json.lookup j "Context") String.of_json
  }
