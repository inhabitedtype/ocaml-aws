open Aws.BaseTypes

type t =
  { auto_scaling_group_name : String.t
  ; scheduled_action_names : ScheduledActionNames.t
  }

let make ~auto_scaling_group_name ~scheduled_action_names () =
  { auto_scaling_group_name; scheduled_action_names }

let parse xml =
  Some
    { auto_scaling_group_name =
        Aws.Xml.required
          "AutoScalingGroupName"
          (Aws.Util.option_bind (Aws.Xml.member "AutoScalingGroupName" xml) String.parse)
    ; scheduled_action_names =
        Aws.Xml.required
          "ScheduledActionNames"
          (Aws.Util.option_bind
             (Aws.Xml.member "ScheduledActionNames" xml)
             ScheduledActionNames.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ScheduledActionNames.member"
              , ScheduledActionNames.to_query v.scheduled_action_names ))
       ; Some
           (Aws.Query.Pair
              ("AutoScalingGroupName", String.to_query v.auto_scaling_group_name))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ("ScheduledActionNames", ScheduledActionNames.to_json v.scheduled_action_names)
       ; Some ("AutoScalingGroupName", String.to_json v.auto_scaling_group_name)
       ])

let of_json j =
  { auto_scaling_group_name =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AutoScalingGroupName"))
  ; scheduled_action_names =
      ScheduledActionNames.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledActionNames"))
  }
