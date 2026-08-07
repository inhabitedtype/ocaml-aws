open Aws.BaseTypes

type t =
  { total_target_capacity : Integer.t option
  ; on_demand_target_capacity : Integer.t option
  ; spot_target_capacity : Integer.t option
  ; default_target_capacity_type : DefaultTargetCapacityType.t option
  ; target_capacity_unit_type : TargetCapacityUnitType.t option
  }

let make
    ?total_target_capacity
    ?on_demand_target_capacity
    ?spot_target_capacity
    ?default_target_capacity_type
    ?target_capacity_unit_type
    () =
  { total_target_capacity
  ; on_demand_target_capacity
  ; spot_target_capacity
  ; default_target_capacity_type
  ; target_capacity_unit_type
  }

let parse xml =
  Some
    { total_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "totalTargetCapacity" xml) Integer.parse
    ; on_demand_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "onDemandTargetCapacity" xml) Integer.parse
    ; spot_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "spotTargetCapacity" xml) Integer.parse
    ; default_target_capacity_type =
        Aws.Util.option_bind
          (Aws.Xml.member "defaultTargetCapacityType" xml)
          DefaultTargetCapacityType.parse
    ; target_capacity_unit_type =
        Aws.Util.option_bind
          (Aws.Xml.member "targetCapacityUnitType" xml)
          TargetCapacityUnitType.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_capacity_unit_type (fun f ->
             Aws.Query.Pair ("TargetCapacityUnitType", TargetCapacityUnitType.to_query f))
       ; Aws.Util.option_map v.default_target_capacity_type (fun f ->
             Aws.Query.Pair
               ("DefaultTargetCapacityType", DefaultTargetCapacityType.to_query f))
       ; Aws.Util.option_map v.spot_target_capacity (fun f ->
             Aws.Query.Pair ("SpotTargetCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.on_demand_target_capacity (fun f ->
             Aws.Query.Pair ("OnDemandTargetCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.total_target_capacity (fun f ->
             Aws.Query.Pair ("TotalTargetCapacity", Integer.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_capacity_unit_type (fun f ->
             "targetCapacityUnitType", TargetCapacityUnitType.to_json f)
       ; Aws.Util.option_map v.default_target_capacity_type (fun f ->
             "defaultTargetCapacityType", DefaultTargetCapacityType.to_json f)
       ; Aws.Util.option_map v.spot_target_capacity (fun f ->
             "spotTargetCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.on_demand_target_capacity (fun f ->
             "onDemandTargetCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.total_target_capacity (fun f ->
             "totalTargetCapacity", Integer.to_json f)
       ])

let of_json j =
  { total_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "totalTargetCapacity") Integer.of_json
  ; on_demand_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "onDemandTargetCapacity") Integer.of_json
  ; spot_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "spotTargetCapacity") Integer.of_json
  ; default_target_capacity_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "defaultTargetCapacityType")
        DefaultTargetCapacityType.of_json
  ; target_capacity_unit_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "targetCapacityUnitType")
        TargetCapacityUnitType.of_json
  }
