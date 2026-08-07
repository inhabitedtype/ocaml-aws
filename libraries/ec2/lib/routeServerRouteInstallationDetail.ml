open Aws.BaseTypes

type t =
  { route_table_id : String.t option
  ; route_installation_status : RouteServerRouteInstallationStatus.t option
  ; route_installation_status_reason : String.t option
  }

let make ?route_table_id ?route_installation_status ?route_installation_status_reason () =
  { route_table_id; route_installation_status; route_installation_status_reason }

let parse xml =
  Some
    { route_table_id =
        Aws.Util.option_bind (Aws.Xml.member "routeTableId" xml) String.parse
    ; route_installation_status =
        Aws.Util.option_bind
          (Aws.Xml.member "routeInstallationStatus" xml)
          RouteServerRouteInstallationStatus.parse
    ; route_installation_status_reason =
        Aws.Util.option_bind
          (Aws.Xml.member "routeInstallationStatusReason" xml)
          String.parse
    }

let to_query v =
  Aws.Query.List
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_installation_status_reason (fun f ->
             Aws.Query.Pair ("RouteInstallationStatusReason", String.to_query f))
       ; Aws.Util.option_map v.route_installation_status (fun f ->
             Aws.Query.Pair
               ("RouteInstallationStatus", RouteServerRouteInstallationStatus.to_query f))
       ; Aws.Util.option_map v.route_table_id (fun f ->
             Aws.Query.Pair ("RouteTableId", String.to_query f))
       ])

let to_json v =
  `Assoc
    (Aws.Util.list_filter_opt
       [ Aws.Util.option_map v.route_installation_status_reason (fun f ->
             "routeInstallationStatusReason", String.to_json f)
       ; Aws.Util.option_map v.route_installation_status (fun f ->
             "routeInstallationStatus", RouteServerRouteInstallationStatus.to_json f)
       ; Aws.Util.option_map v.route_table_id (fun f -> "routeTableId", String.to_json f)
       ])

let of_json j =
  { route_table_id = Aws.Util.option_map (Aws.Json.lookup j "routeTableId") String.of_json
  ; route_installation_status =
      Aws.Util.option_map
        (Aws.Json.lookup j "routeInstallationStatus")
        RouteServerRouteInstallationStatus.of_json
  ; route_installation_status_reason =
      Aws.Util.option_map
        (Aws.Json.lookup j "routeInstallationStatusReason")
        String.of_json
  }
