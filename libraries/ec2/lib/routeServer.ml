open Aws.BaseTypes

type t =
  { route_server_id : String.t option
  ; amazon_side_asn : Long.t option
  ; state : RouteServerState.t option
  ; tags : TagList.t
  ; persist_routes_state : RouteServerPersistRoutesState.t option
  ; persist_routes_duration : Long.t option
  ; sns_notifications_enabled : Boolean.t option
  ; sns_topic_arn : String.t option
  }

let make
    ?route_server_id
    ?amazon_side_asn
    ?state
    ?(tags = [])
    ?persist_routes_state
    ?persist_routes_duration
    ?sns_notifications_enabled
    ?sns_topic_arn
    () =
  { route_server_id
  ; amazon_side_asn
  ; state
  ; tags
  ; persist_routes_state
  ; persist_routes_duration
  ; sns_notifications_enabled
  ; sns_topic_arn
  }

let parse xml =
  Some
    { route_server_id =
        Aws.Util.option_bind (Aws.Xml.member "routeServerId" xml) String.parse
    ; amazon_side_asn =
        Aws.Util.option_bind (Aws.Xml.member "amazonSideAsn" xml) Long.parse
    ; state = Aws.Util.option_bind (Aws.Xml.member "state" xml) RouteServerState.parse
    ; tags =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind (Aws.Xml.member "tagSet" xml) TagList.parse)
    ; persist_routes_state =
        Aws.Util.option_bind
          (Aws.Xml.member "persistRoutesState" xml)
          RouteServerPersistRoutesState.parse
    ; persist_routes_duration =
        Aws.Util.option_bind (Aws.Xml.member "persistRoutesDuration" xml) Long.parse
    ; sns_notifications_enabled =
        Aws.Util.option_bind (Aws.Xml.member "snsNotificationsEnabled" xml) Boolean.parse
    ; sns_topic_arn = Aws.Util.option_bind (Aws.Xml.member "snsTopicArn" xml) String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sns_topic_arn (fun f ->
             Aws.Query.Pair ("SnsTopicArn", String.to_query f))
       ; Aws.Util.option_map v.sns_notifications_enabled (fun f ->
             Aws.Query.Pair ("SnsNotificationsEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.persist_routes_duration (fun f ->
             Aws.Query.Pair ("PersistRoutesDuration", Long.to_query f))
       ; Aws.Util.option_map v.persist_routes_state (fun f ->
             Aws.Query.Pair
               ("PersistRoutesState", RouteServerPersistRoutesState.to_query f))
       ; Some (Aws.Query.Pair ("TagSet", TagList.to_query v.tags))
       ; Aws.Util.option_map v.state (fun f ->
             Aws.Query.Pair ("State", RouteServerState.to_query f))
       ; Aws.Util.option_map v.amazon_side_asn (fun f ->
             Aws.Query.Pair ("AmazonSideAsn", Long.to_query f))
       ; Aws.Util.option_map v.route_server_id (fun f ->
             Aws.Query.Pair ("RouteServerId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.sns_topic_arn (fun f -> "snsTopicArn", String.to_json f)
       ; Aws.Util.option_map v.sns_notifications_enabled (fun f ->
             "snsNotificationsEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.persist_routes_duration (fun f ->
             "persistRoutesDuration", Long.to_json f)
       ; Aws.Util.option_map v.persist_routes_state (fun f ->
             "persistRoutesState", RouteServerPersistRoutesState.to_json f)
       ; Some ("tagSet", TagList.to_json v.tags)
       ; Aws.Util.option_map v.state (fun f -> "state", RouteServerState.to_json f)
       ; Aws.Util.option_map v.amazon_side_asn (fun f -> "amazonSideAsn", Long.to_json f)
       ; Aws.Util.option_map v.route_server_id (fun f ->
             "routeServerId", String.to_json f)
       ])

let of_json j =
  { route_server_id =
      Aws.Util.option_map (Aws.Json.lookup j "routeServerId") String.of_json
  ; amazon_side_asn = Aws.Util.option_map (Aws.Json.lookup j "amazonSideAsn") Long.of_json
  ; state = Aws.Util.option_map (Aws.Json.lookup j "state") RouteServerState.of_json
  ; tags = TagList.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "tagSet"))
  ; persist_routes_state =
      Aws.Util.option_map
        (Aws.Json.lookup j "persistRoutesState")
        RouteServerPersistRoutesState.of_json
  ; persist_routes_duration =
      Aws.Util.option_map (Aws.Json.lookup j "persistRoutesDuration") Long.of_json
  ; sns_notifications_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "snsNotificationsEnabled") Boolean.of_json
  ; sns_topic_arn = Aws.Util.option_map (Aws.Json.lookup j "snsTopicArn") String.of_json
  }
