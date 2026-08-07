open Aws.BaseTypes

type t =
  { dry_run : Boolean.t option
  ; service_id : String.t option
  ; vpc_endpoint_id : String.t option
  ; connection_notification_arn : String.t
  ; connection_events : ValueStringList.t
  ; client_token : String.t option
  }

let make
    ?dry_run
    ?service_id
    ?vpc_endpoint_id
    ~connection_notification_arn
    ~connection_events
    ?client_token
    () =
  { dry_run
  ; service_id
  ; vpc_endpoint_id
  ; connection_notification_arn
  ; connection_events
  ; client_token
  }

let parse xml =
  Some
    { dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; service_id = Aws.Util.option_bind (Aws.Xml.member "ServiceId" xml) String.parse
    ; vpc_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "VpcEndpointId" xml) String.parse
    ; connection_notification_arn =
        Aws.Xml.required
          "ConnectionNotificationArn"
          (Aws.Util.option_bind
             (Aws.Xml.member "ConnectionNotificationArn" xml)
             String.parse)
    ; connection_events =
        Aws.Xml.required
          "ConnectionEvents"
          (Aws.Util.option_bind
             (Aws.Xml.member "ConnectionEvents" xml)
             ValueStringList.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ConnectionEvents", ValueStringList.to_query v.connection_events))
       ; Some
           (Aws.Query.Pair
              ("ConnectionNotificationArn", String.to_query v.connection_notification_arn))
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             Aws.Query.Pair ("VpcEndpointId", String.to_query f))
       ; Aws.Util.option_map v.service_id (fun f ->
             Aws.Query.Pair ("ServiceId", String.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("ConnectionEvents", ValueStringList.to_json v.connection_events)
       ; Some ("ConnectionNotificationArn", String.to_json v.connection_notification_arn)
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             "VpcEndpointId", String.to_json f)
       ; Aws.Util.option_map v.service_id (fun f -> "ServiceId", String.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ])

let of_json j =
  { dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; service_id = Aws.Util.option_map (Aws.Json.lookup j "ServiceId") String.of_json
  ; vpc_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "VpcEndpointId") String.of_json
  ; connection_notification_arn =
      String.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ConnectionNotificationArn"))
  ; connection_events =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "ConnectionEvents"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  }
