open Aws.BaseTypes

type t =
  { launch_template_configs : LaunchTemplateConfigList.t
  ; on_demand_target_capacity : Integer.t option
  ; context : String.t option
  ; spot_fleet_request_id : String.t
  ; target_capacity : Integer.t option
  ; excess_capacity_termination_policy : ExcessCapacityTerminationPolicy.t option
  }

let make
    ?(launch_template_configs = [])
    ?on_demand_target_capacity
    ?context
    ~spot_fleet_request_id
    ?target_capacity
    ?excess_capacity_termination_policy
    () =
  { launch_template_configs
  ; on_demand_target_capacity
  ; context
  ; spot_fleet_request_id
  ; target_capacity
  ; excess_capacity_termination_policy
  }

let parse xml =
  Some
    { launch_template_configs =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LaunchTemplateConfig" xml)
             LaunchTemplateConfigList.parse)
    ; on_demand_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "OnDemandTargetCapacity" xml) Integer.parse
    ; context = Aws.Util.option_bind (Aws.Xml.member "Context" xml) String.parse
    ; spot_fleet_request_id =
        Aws.Xml.required
          "spotFleetRequestId"
          (Aws.Util.option_bind (Aws.Xml.member "spotFleetRequestId" xml) String.parse)
    ; target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "targetCapacity" xml) Integer.parse
    ; excess_capacity_termination_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "excessCapacityTerminationPolicy" xml)
          ExcessCapacityTerminationPolicy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.excess_capacity_termination_policy (fun f ->
             Aws.Query.Pair
               ( "ExcessCapacityTerminationPolicy"
               , ExcessCapacityTerminationPolicy.to_query f ))
       ; Aws.Util.option_map v.target_capacity (fun f ->
             Aws.Query.Pair ("TargetCapacity", Integer.to_query f))
       ; Some
           (Aws.Query.Pair ("SpotFleetRequestId", String.to_query v.spot_fleet_request_id))
       ; Aws.Util.option_map v.context (fun f ->
             Aws.Query.Pair ("Context", String.to_query f))
       ; Aws.Util.option_map v.on_demand_target_capacity (fun f ->
             Aws.Query.Pair ("OnDemandTargetCapacity", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "LaunchTemplateConfig"
              , LaunchTemplateConfigList.to_query v.launch_template_configs ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.excess_capacity_termination_policy (fun f ->
             "excessCapacityTerminationPolicy", ExcessCapacityTerminationPolicy.to_json f)
       ; Aws.Util.option_map v.target_capacity (fun f ->
             "targetCapacity", Integer.to_json f)
       ; Some ("spotFleetRequestId", String.to_json v.spot_fleet_request_id)
       ; Aws.Util.option_map v.context (fun f -> "Context", String.to_json f)
       ; Aws.Util.option_map v.on_demand_target_capacity (fun f ->
             "OnDemandTargetCapacity", Integer.to_json f)
       ; Some
           ( "LaunchTemplateConfig"
           , LaunchTemplateConfigList.to_json v.launch_template_configs )
       ])

let of_json j =
  { launch_template_configs =
      LaunchTemplateConfigList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LaunchTemplateConfig"))
  ; on_demand_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "OnDemandTargetCapacity") Integer.of_json
  ; context = Aws.Util.option_map (Aws.Json.lookup j "Context") String.of_json
  ; spot_fleet_request_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "spotFleetRequestId"))
  ; target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "targetCapacity") Integer.of_json
  ; excess_capacity_termination_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "excessCapacityTerminationPolicy")
        ExcessCapacityTerminationPolicy.of_json
  }
