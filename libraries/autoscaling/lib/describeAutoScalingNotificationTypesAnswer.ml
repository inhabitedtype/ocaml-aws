type t = { auto_scaling_notification_types : AutoScalingNotificationTypes.t }

let make ?(auto_scaling_notification_types = []) () = { auto_scaling_notification_types }

let parse xml =
  Some
    { auto_scaling_notification_types =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "AutoScalingNotificationTypes" xml)
             AutoScalingNotificationTypes.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "AutoScalingNotificationTypes.member"
              , AutoScalingNotificationTypes.to_query v.auto_scaling_notification_types ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "AutoScalingNotificationTypes"
           , AutoScalingNotificationTypes.to_json v.auto_scaling_notification_types )
       ])

let of_json j =
  { auto_scaling_notification_types =
      AutoScalingNotificationTypes.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingNotificationTypes"))
  }
