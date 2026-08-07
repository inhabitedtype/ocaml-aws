open Aws.BaseTypes

type t =
  { scheduled_update_group_actions : ScheduledUpdateGroupActions.t
  ; next_token : String.t option
  }

let make ?(scheduled_update_group_actions = []) ?next_token () =
  { scheduled_update_group_actions; next_token }

let parse xml =
  Some
    { scheduled_update_group_actions =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ScheduledUpdateGroupActions" xml)
             ScheduledUpdateGroupActions.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ScheduledUpdateGroupActions.member"
              , ScheduledUpdateGroupActions.to_query v.scheduled_update_group_actions ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "ScheduledUpdateGroupActions"
           , ScheduledUpdateGroupActions.to_json v.scheduled_update_group_actions )
       ])

let of_json j =
  { scheduled_update_group_actions =
      ScheduledUpdateGroupActions.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ScheduledUpdateGroupActions"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
