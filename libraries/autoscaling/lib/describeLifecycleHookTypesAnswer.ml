type t = { lifecycle_hook_types : AutoScalingNotificationTypes.t }

let make ?(lifecycle_hook_types = []) () = { lifecycle_hook_types }

let parse xml =
  Some
    { lifecycle_hook_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "LifecycleHookTypes" xml)
             AutoScalingNotificationTypes.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "LifecycleHookTypes.member"
              , AutoScalingNotificationTypes.to_query v.lifecycle_hook_types ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "LifecycleHookTypes"
           , AutoScalingNotificationTypes.to_json v.lifecycle_hook_types )
       ])

let of_json j =
  { lifecycle_hook_types =
      AutoScalingNotificationTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "LifecycleHookTypes"))
  }
