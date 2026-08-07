open Aws.BaseTypes

type t =
  { route_server_id : String.t
  ; persist_routes : RouteServerPersistRoutesAction.t option
  ; persist_routes_duration : Long.t option
  ; sns_notifications_enabled : Boolean.t option
  ; dry_run : Boolean.t option
  }

let make
    ~route_server_id
    ?persist_routes
    ?persist_routes_duration
    ?sns_notifications_enabled
    ?dry_run
    () =
  { route_server_id
  ; persist_routes
  ; persist_routes_duration
  ; sns_notifications_enabled
  ; dry_run
  }

let parse xml =
  Some
    { route_server_id =
        Aws.Xml.required
          "RouteServerId"
          (Aws.Util.option_bind (Aws.Xml.member "RouteServerId" xml) String.parse)
    ; persist_routes =
        Aws.Util.option_bind
          (Aws.Xml.member "PersistRoutes" xml)
          RouteServerPersistRoutesAction.parse
    ; persist_routes_duration =
        Aws.Util.option_bind (Aws.Xml.member "PersistRoutesDuration" xml) Long.parse
    ; sns_notifications_enabled =
        Aws.Util.option_bind (Aws.Xml.member "SnsNotificationsEnabled" xml) Boolean.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.sns_notifications_enabled (fun f ->
             Aws.Query.Pair ("SnsNotificationsEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.persist_routes_duration (fun f ->
             Aws.Query.Pair ("PersistRoutesDuration", Long.to_query f))
       ; Aws.Util.option_map v.persist_routes (fun f ->
             Aws.Query.Pair ("PersistRoutes", RouteServerPersistRoutesAction.to_query f))
       ; Some (Aws.Query.Pair ("RouteServerId", String.to_query v.route_server_id))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.sns_notifications_enabled (fun f ->
             "SnsNotificationsEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.persist_routes_duration (fun f ->
             "PersistRoutesDuration", Long.to_json f)
       ; Aws.Util.option_map v.persist_routes (fun f ->
             "PersistRoutes", RouteServerPersistRoutesAction.to_json f)
       ; Some ("RouteServerId", String.to_json v.route_server_id)
       ])

let of_json j =
  { route_server_id =
      String.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "RouteServerId"))
  ; persist_routes =
      Aws.Util.option_map
        (Aws.Json.lookup j "PersistRoutes")
        RouteServerPersistRoutesAction.of_json
  ; persist_routes_duration =
      Aws.Util.option_map (Aws.Json.lookup j "PersistRoutesDuration") Long.of_json
  ; sns_notifications_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "SnsNotificationsEnabled") Boolean.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  }
