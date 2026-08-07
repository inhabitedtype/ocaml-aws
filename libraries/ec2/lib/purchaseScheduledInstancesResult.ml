type t = { scheduled_instance_set : PurchasedScheduledInstanceSet.t }

let make ?(scheduled_instance_set = []) () = { scheduled_instance_set }

let parse xml =
  Some
    { scheduled_instance_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "scheduledInstanceSet" xml)
             PurchasedScheduledInstanceSet.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ScheduledInstanceSet"
              , PurchasedScheduledInstanceSet.to_query v.scheduled_instance_set ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "scheduledInstanceSet"
           , PurchasedScheduledInstanceSet.to_json v.scheduled_instance_set )
       ])

let of_json j =
  { scheduled_instance_set =
      PurchasedScheduledInstanceSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "scheduledInstanceSet"))
  }
