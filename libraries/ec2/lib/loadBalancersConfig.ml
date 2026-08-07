type t =
  { classic_load_balancers_config : ClassicLoadBalancersConfig.t option
  ; target_groups_config : TargetGroupsConfig.t option
  }

let make ?classic_load_balancers_config ?target_groups_config () =
  { classic_load_balancers_config; target_groups_config }

let parse xml =
  Some
    { classic_load_balancers_config =
        Aws.Util.option_bind
          (Aws.Xml.member "classicLoadBalancersConfig" xml)
          ClassicLoadBalancersConfig.parse
    ; target_groups_config =
        Aws.Util.option_bind
          (Aws.Xml.member "targetGroupsConfig" xml)
          TargetGroupsConfig.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_groups_config (fun f ->
             Aws.Query.Pair ("TargetGroupsConfig", TargetGroupsConfig.to_query f))
       ; Aws.Util.option_map v.classic_load_balancers_config (fun f ->
             Aws.Query.Pair
               ("ClassicLoadBalancersConfig", ClassicLoadBalancersConfig.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.target_groups_config (fun f ->
             "targetGroupsConfig", TargetGroupsConfig.to_json f)
       ; Aws.Util.option_map v.classic_load_balancers_config (fun f ->
             "classicLoadBalancersConfig", ClassicLoadBalancersConfig.to_json f)
       ])

let of_json j =
  { classic_load_balancers_config =
      Aws.Util.option_map
        (Aws.Json.lookup j "classicLoadBalancersConfig")
        ClassicLoadBalancersConfig.of_json
  ; target_groups_config =
      Aws.Util.option_map
        (Aws.Json.lookup j "targetGroupsConfig")
        TargetGroupsConfig.of_json
  }
