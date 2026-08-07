type t =
  { failed_scheduled_update_group_actions : FailedScheduledUpdateGroupActionRequests.t }

let make ?(failed_scheduled_update_group_actions = []) () =
  { failed_scheduled_update_group_actions }

let parse xml =
  Some
    { failed_scheduled_update_group_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "FailedScheduledUpdateGroupActions" xml)
             FailedScheduledUpdateGroupActionRequests.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "FailedScheduledUpdateGroupActions.member"
              , FailedScheduledUpdateGroupActionRequests.to_query
                  v.failed_scheduled_update_group_actions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "FailedScheduledUpdateGroupActions"
           , FailedScheduledUpdateGroupActionRequests.to_json
               v.failed_scheduled_update_group_actions )
       ])

let of_json j =
  { failed_scheduled_update_group_actions =
      FailedScheduledUpdateGroupActionRequests.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "FailedScheduledUpdateGroupActions"))
  }
