open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; scheduled_update_group_actions : ScheduledUpdateGroupActionRequests.t
  }

let make ~auto_scaling_group_name ~scheduled_update_group_actions () =
  { auto_scaling_group_name; scheduled_update_group_actions }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; scheduled_update_group_actions =
        Aws.Xml.required
          "ScheduledUpdateGroupActions"
          (Aws.Util.option_bind
             (Aws.Xml.member "ScheduledUpdateGroupActions" xml)
             ScheduledUpdateGroupActionRequests.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ScheduledUpdateGroupActions.member"
              , ScheduledUpdateGroupActionRequests.to_query
                  v.scheduled_update_group_actions ))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ScheduledUpdateGroupActions"
           , ScheduledUpdateGroupActionRequests.to_json v.scheduled_update_group_actions
           )
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; scheduled_update_group_actions =
      ScheduledUpdateGroupActionRequests.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledUpdateGroupActions"))
  }
