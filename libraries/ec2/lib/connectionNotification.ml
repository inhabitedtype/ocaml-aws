open Aws.BaseTypes

type t =
  { connection_notification_id : String.t option
  ; service_id : String.t option
  ; vpc_endpoint_id : String.t option
  ; connection_notification_type : ConnectionNotificationType.t option
  ; connection_notification_arn : String.t option
  ; connection_events : ValueStringList.t
  ; connection_notification_state : ConnectionNotificationState.t option
  ; service_region : String.t option
  }

let make
    ?connection_notification_id
    ?service_id
    ?vpc_endpoint_id
    ?connection_notification_type
    ?connection_notification_arn
    ?(connection_events = [])
    ?connection_notification_state
    ?service_region
    () =
  { connection_notification_id
  ; service_id
  ; vpc_endpoint_id
  ; connection_notification_type
  ; connection_notification_arn
  ; connection_events
  ; connection_notification_state
  ; service_region
  }

let parse xml =
  Some
    { connection_notification_id =
        Aws.Util.option_bind (Aws.Xml.member "connectionNotificationId" xml) String.parse
    ; service_id = Aws.Util.option_bind (Aws.Xml.member "serviceId" xml) String.parse
    ; vpc_endpoint_id =
        Aws.Util.option_bind (Aws.Xml.member "vpcEndpointId" xml) String.parse
    ; connection_notification_type =
        Aws.Util.option_bind
          (Aws.Xml.member "connectionNotificationType" xml)
          ConnectionNotificationType.parse
    ; connection_notification_arn =
        Aws.Util.option_bind (Aws.Xml.member "connectionNotificationArn" xml) String.parse
    ; connection_events =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "connectionEvents" xml)
             ValueStringList.parse)
    ; connection_notification_state =
        Aws.Util.option_bind
          (Aws.Xml.member "connectionNotificationState" xml)
          ConnectionNotificationState.parse
    ; service_region =
        Aws.Util.option_bind (Aws.Xml.member "serviceRegion" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_region (fun f ->
             Aws.Query.Pair ("ServiceRegion", String.to_query f))
       ; Aws.Util.option_map v.connection_notification_state (fun f ->
             Aws.Query.Pair
               ("ConnectionNotificationState", ConnectionNotificationState.to_query f))
       ; Some
           (Aws.Query.Pair
              ("ConnectionEvents", ValueStringList.to_query v.connection_events))
       ; Aws.Util.option_map v.connection_notification_arn (fun f ->
             Aws.Query.Pair ("ConnectionNotificationArn", String.to_query f))
       ; Aws.Util.option_map v.connection_notification_type (fun f ->
             Aws.Query.Pair
               ("ConnectionNotificationType", ConnectionNotificationType.to_query f))
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             Aws.Query.Pair ("VpcEndpointId", String.to_query f))
       ; Aws.Util.option_map v.service_id (fun f ->
             Aws.Query.Pair ("ServiceId", String.to_query f))
       ; Aws.Util.option_map v.connection_notification_id (fun f ->
             Aws.Query.Pair ("ConnectionNotificationId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.service_region (fun f -> "serviceRegion", String.to_json f)
       ; Aws.Util.option_map v.connection_notification_state (fun f ->
             "connectionNotificationState", ConnectionNotificationState.to_json f)
       ; Some ("connectionEvents", ValueStringList.to_json v.connection_events)
       ; Aws.Util.option_map v.connection_notification_arn (fun f ->
             "connectionNotificationArn", String.to_json f)
       ; Aws.Util.option_map v.connection_notification_type (fun f ->
             "connectionNotificationType", ConnectionNotificationType.to_json f)
       ; Aws.Util.option_map v.vpc_endpoint_id (fun f ->
             "vpcEndpointId", String.to_json f)
       ; Aws.Util.option_map v.service_id (fun f -> "serviceId", String.to_json f)
       ; Aws.Util.option_map v.connection_notification_id (fun f ->
             "connectionNotificationId", String.to_json f)
       ])

let of_json j =
  { connection_notification_id =
      Aws.Util.option_map (Aws.Json.lookup j "connectionNotificationId") String.of_json
  ; service_id = Aws.Util.option_map (Aws.Json.lookup j "serviceId") String.of_json
  ; vpc_endpoint_id =
      Aws.Util.option_map (Aws.Json.lookup j "vpcEndpointId") String.of_json
  ; connection_notification_type =
      Aws.Util.option_map
        (Aws.Json.lookup j "connectionNotificationType")
        ConnectionNotificationType.of_json
  ; connection_notification_arn =
      Aws.Util.option_map (Aws.Json.lookup j "connectionNotificationArn") String.of_json
  ; connection_events =
      ValueStringList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "connectionEvents"))
  ; connection_notification_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "connectionNotificationState")
        ConnectionNotificationState.of_json
  ; service_region =
      Aws.Util.option_map (Aws.Json.lookup j "serviceRegion") String.of_json
  }
