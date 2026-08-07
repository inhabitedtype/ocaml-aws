open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; connection_notification_id : String.t
  ; connection_notification_arn : String.t option
  ; connection_events : ValueStringList.t
  }

let make
    ?dry_run
    ~connection_notification_id
    ?connection_notification_arn
    ?(connection_events = [])
    () =
  { dry_run; connection_notification_id; connection_notification_arn; connection_events }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; connection_notification_id =
        Aws.Xml.required
          "ConnectionNotificationId"
          (Aws.Util.option_bind
             (Aws.Xml.member "ConnectionNotificationId" xml)
             String.parse)
    ; connection_notification_arn =
        Aws.Util.option_bind (Aws.Xml.member "ConnectionNotificationArn" xml) String.parse
    ; connection_events =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "ConnectionEvents" xml)
             ValueStringList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("ConnectionEvents", ValueStringList.to_query v.connection_events))
       ; Aws.Util.option_map v.connection_notification_arn (fun f ->
             Aws.Query.Pair ("ConnectionNotificationArn", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ConnectionNotificationId", String.to_query v.connection_notification_id))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("ConnectionEvents", ValueStringList.to_json v.connection_events)
       ; Aws.Util.option_map v.connection_notification_arn (fun f ->
             "ConnectionNotificationArn", String.to_json f)
       ; Some ("ConnectionNotificationId", String.to_json v.connection_notification_id)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; connection_notification_id =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ConnectionNotificationId"))
  ; connection_notification_arn =
      Aws.Util.option_map (Aws.Json.lookup j "ConnectionNotificationArn") String.of_json
  ; connection_events =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ConnectionEvents"))
  }
