open Aws.BaseTypes

type t =
  { notification_configurations : NotificationConfigurations.t
  ; next_token : String.t option
  }

let make ~notification_configurations ?next_token () =
  { notification_configurations; next_token }

let parse xml =
  Some
    { notification_configurations =
        Aws.Xml.required
          "NotificationConfigurations"
          (Aws.Util.option_bind
             (Aws.Xml.member "NotificationConfigurations" xml)
             NotificationConfigurations.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "NextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "NotificationConfigurations.member"
              , NotificationConfigurations.to_query v.notification_configurations ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "NextToken", String.to_json f)
       ; Some
           ( "NotificationConfigurations"
           , NotificationConfigurations.to_json v.notification_configurations )
       ])

let of_json j =
  { notification_configurations =
      NotificationConfigurations.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "NotificationConfigurations"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "NextToken") String.of_json
  }
