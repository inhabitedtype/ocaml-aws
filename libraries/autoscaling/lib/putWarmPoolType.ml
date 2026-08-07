open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; max_group_prepared_capacity : Integer.t option
  ; min_size : Integer.t option
  ; pool_state : WarmPoolState.t option
  ; instance_reuse_policy : InstanceReusePolicy.t option
  }

let make
    ~auto_scaling_group_name
    ?max_group_prepared_capacity
    ?min_size
    ?pool_state
    ?instance_reuse_policy
    () =
  { auto_scaling_group_name
  ; max_group_prepared_capacity
  ; min_size
  ; pool_state
  ; instance_reuse_policy
  }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; max_group_prepared_capacity =
        Aws.Util.option_bind (Aws.Xml.member "MaxGroupPreparedCapacity" xml) Integer.parse
    ; min_size = Aws.Util.option_bind (Aws.Xml.member "MinSize" xml) Integer.parse
    ; pool_state =
        Aws.Util.option_bind (Aws.Xml.member "PoolState" xml) WarmPoolState.parse
    ; instance_reuse_policy =
        Aws.Util.option_bind
          (Aws.Xml.member "InstanceReusePolicy" xml)
          InstanceReusePolicy.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_reuse_policy (fun f ->
             Aws.Query.Pair ("InstanceReusePolicy", InstanceReusePolicy.to_query f))
       ; Aws.Util.option_map v.pool_state (fun f ->
             Aws.Query.Pair ("PoolState", WarmPoolState.to_query f))
       ; Aws.Util.option_map v.min_size (fun f ->
             Aws.Query.Pair ("MinSize", Integer.to_query f))
       ; Aws.Util.option_map v.max_group_prepared_capacity (fun f ->
             Aws.Query.Pair ("MaxGroupPreparedCapacity", Integer.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_reuse_policy (fun f ->
             "InstanceReusePolicy", InstanceReusePolicy.to_json f)
       ; Aws.Util.option_map v.pool_state (fun f -> "PoolState", WarmPoolState.to_json f)
       ; Aws.Util.option_map v.min_size (fun f -> "MinSize", Integer.to_json f)
       ; Aws.Util.option_map v.max_group_prepared_capacity (fun f ->
             "MaxGroupPreparedCapacity", Integer.to_json f)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; max_group_prepared_capacity =
      Aws.Util.option_map (Aws.Json.lookup j "MaxGroupPreparedCapacity") Integer.of_json
  ; min_size = Aws.Util.option_map (Aws.Json.lookup j "MinSize") Integer.of_json
  ; pool_state = Aws.Util.option_map (Aws.Json.lookup j "PoolState") WarmPoolState.of_json
  ; instance_reuse_policy =
      Aws.Util.option_map
        (Aws.Json.lookup j "InstanceReusePolicy")
        InstanceReusePolicy.of_json
  }
