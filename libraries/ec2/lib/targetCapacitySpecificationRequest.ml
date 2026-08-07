open Aws.BaseTypes

type t =
  { total_target_capacity : Integer.t
  ; on_demand_target_capacity : Integer.t option
  ; spot_target_capacity : Integer.t option
  ; default_target_capacity_type : DefaultTargetCapacityType.t option
  ; target_capacity_unit_type : TargetCapacityUnitType.t option
  }

let make
    ~total_target_capacity
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
        Aws.Xml.required
          "TotalTargetCapacity"
          (Aws.Util.option_bind (Aws.Xml.member "TotalTargetCapacity" xml) Integer.parse)
    ; on_demand_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "OnDemandTargetCapacity" xml) Integer.parse
    ; spot_target_capacity =
        Aws.Util.option_bind (Aws.Xml.member "SpotTargetCapacity" xml) Integer.parse
    ; default_target_capacity_type =
        Aws.Util.option_bind
          (Aws.Xml.member "DefaultTargetCapacityType" xml)
          DefaultTargetCapacityType.parse
    ; target_capacity_unit_type =
        Aws.Util.option_bind
          (Aws.Xml.member "TargetCapacityUnitType" xml)
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
       ; Some
           (Aws.Query.Pair
              ("TotalTargetCapacity", Integer.to_query v.total_target_capacity))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_capacity_unit_type (fun f ->
             "TargetCapacityUnitType", TargetCapacityUnitType.to_json f)
       ; Aws.Util.option_map v.default_target_capacity_type (fun f ->
             "DefaultTargetCapacityType", DefaultTargetCapacityType.to_json f)
       ; Aws.Util.option_map v.spot_target_capacity (fun f ->
             "SpotTargetCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.on_demand_target_capacity (fun f ->
             "OnDemandTargetCapacity", Integer.to_json f)
       ; Some ("TotalTargetCapacity", Integer.to_json v.total_target_capacity)
       ])

let of_json j =
  { total_target_capacity =
      Integer.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "TotalTargetCapacity"))
  ; on_demand_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "OnDemandTargetCapacity") Integer.of_json
  ; spot_target_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "SpotTargetCapacity") Integer.of_json
  ; default_target_capacity_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "DefaultTargetCapacityType")
        DefaultTargetCapacityType.of_json
  ; target_capacity_unit_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "TargetCapacityUnitType")
        TargetCapacityUnitType.of_json
  }
