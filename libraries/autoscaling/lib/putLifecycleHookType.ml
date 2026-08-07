open Aws.BaseTypes

type t =
  { lifecycle_hook_name : String.t
  ; auto_scaling_group_name : String.t
  ; lifecycle_transition : String.t option
  ; role_a_r_n : String.t option
  ; notification_target_a_r_n : String.t option
  ; notification_metadata : String.t option
  ; heartbeat_timeout : Integer.t option
  ; default_result : String.t option
  }

let make
    ~lifecycle_hook_name
    ~auto_scaling_group_name
    ?lifecycle_transition
    ?role_a_r_n
    ?notification_target_a_r_n
    ?notification_metadata
    ?heartbeat_timeout
    ?default_result
    () =
  { lifecycle_hook_name
  ; auto_scaling_group_name
  ; lifecycle_transition
  ; role_a_r_n
  ; notification_target_a_r_n
  ; notification_metadata
  ; heartbeat_timeout
  ; default_result
  }

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
    ; lifecycle_transition =
        Aws.Util.option_bind (Aws.Xml.member "LifecycleTransition" xml) String.parse
    ; role_a_r_n = Aws.Util.option_bind (Aws.Xml.member "RoleARN" xml) String.parse
    ; notification_target_a_r_n =
        Aws.Util.option_bind (Aws.Xml.member "NotificationTargetARN" xml) String.parse
    ; notification_metadata =
        Aws.Util.option_bind (Aws.Xml.member "NotificationMetadata" xml) String.parse
    ; heartbeat_timeout =
        Aws.Util.option_bind (Aws.Xml.member "HeartbeatTimeout" xml) Integer.parse
    ; default_result =
        Aws.Util.option_bind (Aws.Xml.member "DefaultResult" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.default_result (fun f ->
             Aws.Query.Pair ("DefaultResult", String.to_query f))
       ; Aws.Util.option_map v.heartbeat_timeout (fun f ->
             Aws.Query.Pair ("HeartbeatTimeout", Integer.to_query f))
       ; Aws.Util.option_map v.notification_metadata (fun f ->
             Aws.Query.Pair ("NotificationMetadata", String.to_query f))
       ; Aws.Util.option_map v.notification_target_a_r_n (fun f ->
             Aws.Query.Pair ("NotificationTargetARN", String.to_query f))
       ; Aws.Util.option_map v.role_a_r_n (fun f ->
             Aws.Query.Pair ("RoleARN", String.to_query f))
       ; Aws.Util.option_map v.lifecycle_transition (fun f ->
             Aws.Query.Pair ("LifecycleTransition", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ; Some
           (Aws.Query.Pair ("LifecycleHookName", String.to_query v.lifecycle_hook_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.default_result (fun f -> "DefaultResult", String.to_json f)
       ; Aws.Util.option_map v.heartbeat_timeout (fun f ->
             "HeartbeatTimeout", Integer.to_json f)
       ; Aws.Util.option_map v.notification_metadata (fun f ->
             "NotificationMetadata", String.to_json f)
       ; Aws.Util.option_map v.notification_target_a_r_n (fun f ->
             "NotificationTargetARN", String.to_json f)
       ; Aws.Util.option_map v.role_a_r_n (fun f -> "RoleARN", String.to_json f)
       ; Aws.Util.option_map v.lifecycle_transition (fun f ->
             "LifecycleTransition", String.to_json f)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ; Some ("LifecycleHookName", String.to_json v.lifecycle_hook_name)
       ])

let of_json j =
  { lifecycle_hook_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookName"))
  ; auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; lifecycle_transition =
      Aws.Util.option_map (Aws.Json.lookup j "LifecycleTransition") String.of_json
  ; role_a_r_n = Aws.Util.option_map (Aws.Json.lookup j "RoleARN") String.of_json
  ; notification_target_a_r_n =
      Aws.Util.option_map (Aws.Json.lookup j "NotificationTargetARN") String.of_json
  ; notification_metadata =
      Aws.Util.option_map (Aws.Json.lookup j "NotificationMetadata") String.of_json
  ; heartbeat_timeout =
      Aws.Util.option_map (Aws.Json.lookup j "HeartbeatTimeout") Integer.of_json
  ; default_result =
      Aws.Util.option_map (Aws.Json.lookup j "DefaultResult") String.of_json
  }
