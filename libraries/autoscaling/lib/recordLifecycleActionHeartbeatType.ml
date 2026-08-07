open Aws.BaseTypes

type t =
  { lifecycle_hook_name : String.t
  ; auto_scaling_group_name : String.t
  ; lifecycle_action_token : String.t option
  ; instance_id : String.t option
  }

let make
    ~lifecycle_hook_name
    ~auto_scaling_group_name
    ?lifecycle_action_token
    ?instance_id
    () =
  { lifecycle_hook_name; auto_scaling_group_name; lifecycle_action_token; instance_id }

let parse xml =
  Some
    { lifecycle_hook_name =
        Aws.Xml.required
          "LifecycleHookName"
          (Aws.Util.option_bind (Aws.Xml.member "LifecycleHookName" xml) String.parse)
    ; auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; lifecycle_action_token =
        Aws.Util.option_bind (Aws.Xml.member "LifecycleActionToken" xml) String.parse
    ; instance_id = Aws.Util.option_bind (Aws.Xml.member "InstanceId" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f ->
             Aws.Query.Pair ("InstanceId", String.to_query f))
       ; Aws.Util.option_map v.lifecycle_action_token (fun f ->
             Aws.Query.Pair ("LifecycleActionToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ; Some
           (Aws.Query.Pair ("LifecycleHookName", String.to_query v.lifecycle_hook_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.instance_id (fun f -> "InstanceId", String.to_json f)
       ; Aws.Util.option_map v.lifecycle_action_token (fun f ->
             "LifecycleActionToken", String.to_json f)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ; Some ("LifecycleHookName", String.to_json v.lifecycle_hook_name)
       ])

let of_json j =
  { lifecycle_hook_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookName"))
  ; auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; lifecycle_action_token =
      Aws.Util.option_map (Aws.Json.lookup j "LifecycleActionToken") String.of_json
  ; instance_id = Aws.Util.option_map (Aws.Json.lookup j "InstanceId") String.of_json
  }
