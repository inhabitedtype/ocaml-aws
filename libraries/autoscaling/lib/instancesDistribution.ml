open Aws.BaseTypes

type t =
  { on_demand_allocation_strategy : String.t option
  ; on_demand_base_capacity : Integer.t option
  ; on_demand_percentage_above_base_capacity : Integer.t option
  ; spot_allocation_strategy : String.t option
  ; spot_instance_pools : Integer.t option
  ; spot_max_price : String.t option
  }

let make
    ?on_demand_allocation_strategy
    ?on_demand_base_capacity
    ?on_demand_percentage_above_base_capacity
    ?spot_allocation_strategy
    ?spot_instance_pools
    ?spot_max_price
    () =
  { on_demand_allocation_strategy
  ; on_demand_base_capacity
  ; on_demand_percentage_above_base_capacity
  ; spot_allocation_strategy
  ; spot_instance_pools
  ; spot_max_price
  }

let parse xml =
  Some
    { on_demand_allocation_strategy =
        Aws.Util.option_bind
          (Aws.Xml.member "OnDemandAllocationStrategy" xml)
          String.parse
    ; on_demand_base_capacity =
        Aws.Util.option_bind (Aws.Xml.member "OnDemandBaseCapacity" xml) Integer.parse
    ; on_demand_percentage_above_base_capacity =
        Aws.Util.option_bind
          (Aws.Xml.member "OnDemandPercentageAboveBaseCapacity" xml)
          Integer.parse
    ; spot_allocation_strategy =
        Aws.Util.option_bind (Aws.Xml.member "SpotAllocationStrategy" xml) String.parse
    ; spot_instance_pools =
        Aws.Util.option_bind (Aws.Xml.member "SpotInstancePools" xml) Integer.parse
    ; spot_max_price =
        Aws.Util.option_bind (Aws.Xml.member "SpotMaxPrice" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_max_price (fun f ->
             Aws.Query.Pair ("SpotMaxPrice", String.to_query f))
       ; Aws.Util.option_map v.spot_instance_pools (fun f ->
             Aws.Query.Pair ("SpotInstancePools", Integer.to_query f))
       ; Aws.Util.option_map v.spot_allocation_strategy (fun f ->
             Aws.Query.Pair ("SpotAllocationStrategy", String.to_query f))
       ; Aws.Util.option_map v.on_demand_percentage_above_base_capacity (fun f ->
             Aws.Query.Pair ("OnDemandPercentageAboveBaseCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.on_demand_base_capacity (fun f ->
             Aws.Query.Pair ("OnDemandBaseCapacity", Integer.to_query f))
       ; Aws.Util.option_map v.on_demand_allocation_strategy (fun f ->
             Aws.Query.Pair ("OnDemandAllocationStrategy", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.spot_max_price (fun f -> "SpotMaxPrice", String.to_json f)
       ; Aws.Util.option_map v.spot_instance_pools (fun f ->
             "SpotInstancePools", Integer.to_json f)
       ; Aws.Util.option_map v.spot_allocation_strategy (fun f ->
             "SpotAllocationStrategy", String.to_json f)
       ; Aws.Util.option_map v.on_demand_percentage_above_base_capacity (fun f ->
             "OnDemandPercentageAboveBaseCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.on_demand_base_capacity (fun f ->
             "OnDemandBaseCapacity", Integer.to_json f)
       ; Aws.Util.option_map v.on_demand_allocation_strategy (fun f ->
             "OnDemandAllocationStrategy", String.to_json f)
       ])

let of_json j =
  { on_demand_allocation_strategy =
      Aws.Util.option_map (Aws.Json.lookup j "OnDemandAllocationStrategy") String.of_json
  ; on_demand_base_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "OnDemandBaseCapacity") Integer.of_json
  ; on_demand_percentage_above_base_capacity =
      Aws.Util.option_map
        (Aws.Json.lookup j "OnDemandPercentageAboveBaseCapacity")
        Integer.of_json
  ; spot_allocation_strategy =
      Aws.Util.option_map (Aws.Json.lookup j "SpotAllocationStrategy") String.of_json
  ; spot_instance_pools =
      Aws.Util.option_map (Aws.Json.lookup j "SpotInstancePools") Integer.of_json
  ; spot_max_price = Aws.Util.option_map (Aws.Json.lookup j "SpotMaxPrice") String.of_json
  }
