open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; lifecycle_hook_names : LifecycleHookNames.t
  }

let make ~auto_scaling_group_name ?(lifecycle_hook_names = []) () =
  { auto_scaling_group_name; lifecycle_hook_names }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; lifecycle_hook_names =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LifecycleHookNames" xml)
             LifecycleHookNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "LifecycleHookNames.member"
              , LifecycleHookNames.to_query v.lifecycle_hook_names ))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("LifecycleHookNames", LifecycleHookNames.to_json v.lifecycle_hook_names)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; lifecycle_hook_names =
      LifecycleHookNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookNames"))
  }
