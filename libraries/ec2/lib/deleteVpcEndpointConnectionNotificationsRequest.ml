open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; connection_notification_ids : ConnectionNotificationIdsList.t
  }

let make ?dry_run ~connection_notification_ids () =
  { dry_run; connection_notification_ids }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; connection_notification_ids =
        Aws.Xml.required
          "ConnectionNotificationId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ConnectionNotificationId" xml)
             ConnectionNotificationIdsList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ( "ConnectionNotificationId"
              , ConnectionNotificationIdsList.to_query v.connection_notification_ids ))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some
           ( "ConnectionNotificationId"
           , ConnectionNotificationIdsList.to_json v.connection_notification_ids )
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; connection_notification_ids =
      ConnectionNotificationIdsList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ConnectionNotificationId"))
  }
