open Aws.BaseTypes

type t =
  { lifecycle_hook_name : String.t
  ; auto_scaling_group_name : String.t
  }

let make ~lifecycle_hook_name ~auto_scaling_group_name () =
  { lifecycle_hook_name; auto_scaling_group_name }

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
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ; Some
           (Aws.Query.Pair ("LifecycleHookName", String.to_query v.lifecycle_hook_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ; Some ("LifecycleHookName", String.to_json v.lifecycle_hook_name)
       ])

let of_json j =
  { lifecycle_hook_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookName"))
  ; auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  }
