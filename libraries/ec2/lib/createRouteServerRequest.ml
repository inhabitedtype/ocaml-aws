open Aws.BaseTypes

type t =
  { amazon_side_asn : Long.t
  ; client_token : String.t option
  ; dry_run : Boolean.t option
  ; persist_routes : RouteServerPersistRoutesAction.t option
  ; persist_routes_duration : Long.t option
  ; sns_notifications_enabled : Boolean.t option
  ; tag_specifications : TagSpecificationList.t
  }

let make
    ~amazon_side_asn
    ?client_token
    ?dry_run
    ?persist_routes
    ?persist_routes_duration
    ?sns_notifications_enabled
    ?(tag_specifications = [])
    () =
  { amazon_side_asn
  ; client_token
  ; dry_run
  ; persist_routes
  ; persist_routes_duration
  ; sns_notifications_enabled
  ; tag_specifications
  }

let parse xml =
  Some
    { amazon_side_asn =
        Aws.Xml.required
          "AmazonSideAsn"
          (Aws.Util.option_bind (Aws.Xml.member "AmazonSideAsn" xml) Long.parse)
    ; client_token = Aws.Util.option_bind (Aws.Xml.member "ClientToken" xml) String.parse
    ; dry_run = Aws.Util.option_bind (Aws.Xml.member "DryRun" xml) Boolean.parse
    ; persist_routes =
        Aws.Util.option_bind
          (Aws.Xml.member "PersistRoutes" xml)
          RouteServerPersistRoutesAction.parse
    ; persist_routes_duration =
        Aws.Util.option_bind (Aws.Xml.member "PersistRoutesDuration" xml) Long.parse
    ; sns_notifications_enabled =
        Aws.Util.option_bind (Aws.Xml.member "SnsNotificationsEnabled" xml) Boolean.parse
    ; tag_specifications =
        Aws.Util.of_option
          []
          (Aws.Util.option_bind
             (Aws.Xml.member "TagSpecification" xml)
             TagSpecificationList.parse)
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Some
           (Aws.Query.Pair
              ("TagSpecification", TagSpecificationList.to_query v.tag_specifications))
       ; Aws.Util.option_map v.sns_notifications_enabled (fun f ->
             Aws.Query.Pair ("SnsNotificationsEnabled", Boolean.to_query f))
       ; Aws.Util.option_map v.persist_routes_duration (fun f ->
             Aws.Query.Pair ("PersistRoutesDuration", Long.to_query f))
       ; Aws.Util.option_map v.persist_routes (fun f ->
             Aws.Query.Pair ("PersistRoutes", RouteServerPersistRoutesAction.to_query f))
       ; Aws.Util.option_map v.dry_run (fun f ->
             Aws.Query.Pair ("DryRun", Boolean.to_query f))
       ; Aws.Util.option_map v.client_token (fun f ->
             Aws.Query.Pair ("ClientToken", String.to_query f))
       ; Some (Aws.Query.Pair ("AmazonSideAsn", Long.to_query v.amazon_side_asn))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Some ("TagSpecification", TagSpecificationList.to_json v.tag_specifications)
       ; Aws.Util.option_map v.sns_notifications_enabled (fun f ->
             "SnsNotificationsEnabled", Boolean.to_json f)
       ; Aws.Util.option_map v.persist_routes_duration (fun f ->
             "PersistRoutesDuration", Long.to_json f)
       ; Aws.Util.option_map v.persist_routes (fun f ->
             "PersistRoutes", RouteServerPersistRoutesAction.to_json f)
       ; Aws.Util.option_map v.dry_run (fun f -> "DryRun", Boolean.to_json f)
       ; Aws.Util.option_map v.client_token (fun f -> "ClientToken", String.to_json f)
       ; Some ("AmazonSideAsn", Long.to_json v.amazon_side_asn)
       ])

let of_json j =
  { amazon_side_asn =
      Long.of_json (Aws.Util.of_option_exn (Aws.Json.lookup j "AmazonSideAsn"))
  ; client_token = Aws.Util.option_map (Aws.Json.lookup j "ClientToken") String.of_json
  ; dry_run = Aws.Util.option_map (Aws.Json.lookup j "DryRun") Boolean.of_json
  ; persist_routes =
      Aws.Util.option_map
        (Aws.Json.lookup j "PersistRoutes")
        RouteServerPersistRoutesAction.of_json
  ; persist_routes_duration =
      Aws.Util.option_map (Aws.Json.lookup j "PersistRoutesDuration") Long.of_json
  ; sns_notifications_enabled =
      Aws.Util.option_map (Aws.Json.lookup j "SnsNotificationsEnabled") Boolean.of_json
  ; tag_specifications =
      TagSpecificationList.of_json
        (Aws.Util.of_option_exn (Aws.Json.lookup j "TagSpecification"))
  }
