type t = { failed_scheduled_actions : FailedScheduledUpdateGroupActionRequests.t }

let make ?(failed_scheduled_actions = []) () = { failed_scheduled_actions }

let parse xml =
  Some
    { failed_scheduled_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "FailedScheduledActions" xml)
             FailedScheduledUpdateGroupActionRequests.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "FailedScheduledActions.member"
              , FailedScheduledUpdateGroupActionRequests.to_query
                  v.failed_scheduled_actions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "FailedScheduledActions"
           , FailedScheduledUpdateGroupActionRequests.to_json v.failed_scheduled_actions
           )
       ])

let of_json j =
  { failed_scheduled_actions =
      FailedScheduledUpdateGroupActionRequests.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "FailedScheduledActions"))
  }
