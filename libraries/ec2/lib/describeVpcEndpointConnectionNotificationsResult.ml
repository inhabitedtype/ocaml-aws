open Aws.BaseTypes

type t =
  { connection_notification_set : ConnectionNotificationSet.t
  ; next_token : String.t option
  }

let make ?(connection_notification_set = []) ?next_token () =
  { connection_notification_set; next_token }

let parse xml =
  Some
    { connection_notification_set =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "connectionNotificationSet" xml)
             ConnectionNotificationSet.parse)
    ; next_token = Aws.Util.option_bind (Aws.Xml.member "nextToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f ->
             Aws.Query.Pair ("NextToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ( "ConnectionNotificationSet"
              , ConnectionNotificationSet.to_query v.connection_notification_set ))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.next_token (fun f -> "nextToken", String.to_json f)
       ; Some
           ( "connectionNotificationSet"
           , ConnectionNotificationSet.to_json v.connection_notification_set )
       ])

let of_json j =
  { connection_notification_set =
      ConnectionNotificationSet.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "connectionNotificationSet"))
  ; next_token = Aws.Util.option_map (Aws.Json.lookup j "nextToken") String.of_json
  }
